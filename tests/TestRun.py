from dataclasses import dataclass, field
import os
import re
import subprocess
import sys
from rich.console import Console


WorkDir = os.path.dirname(__file__)
CompilerPath = os.path.dirname(WorkDir) + "/bin/LunarTcc"
console = Console()
SaveTemps = False
testSet = []
failedTests = []
testsCount = 0
passedTestsCount = 0


@dataclass
class Context:
    Arch: str = ""
    RunCommand: str = ""
    FunctionDeclarations: list = field(default_factory=list)
    TestCases: list = field(default_factory=list)
    CheckList: list = field(default_factory=list)
    CheckNotList: list = field(default_factory=list)
    RunTest: bool = False
    RunFailTest: bool = False
    CompileTest: bool = False
    NegativeTest: bool = False
    ExtraCompileFlags: str = ""


def creatFiles(fileName, context):
    textFile = open(fileName, "w")
    textFile.write(context)
    textFile.close()


def cleanCacheFiles():
    if os.path.exists("testMain.c"):
        os.remove("testMain.c")
    if os.path.exists("test.s"):
        os.remove("test.s")
    if os.path.exists("test"):
        os.remove("test")


def checkFile(fileName):
    context = Context()

    with open(fileName) as file:
        for line in file:
            m = re.search(r"(?:/{2}|#) *RUN: (.*)", line)
            if m:
                context.Arch = m.group(1).lower()
                context.RunCommand = "qemu-" + context.Arch
                context.RunTest = True
                continue

            m = re.search(r'(?:/{2}|#) *RUN-FAIL: (.*)', line)
            if m:
                context.Arch = m.group(1).lower()
                context.RunFailTest = True
                continue

            m = re.search(r'(?:/{2}|#) *COMPILE-TEST', line)
            if m:
                context.CompileTest = True
                continue

            m = re.search(r"(?:/{2}|#) *FUNC-DECL: (.*)", line)
            if m:
                context.FunctionDeclarations.append(m.group(1))
                continue

            m = re.search(r'(?:/{2}|#) *CHECK:\s*(.*)', line)
            if m:
                context.CheckList.append(m.group(1))
                continue

            m = re.search(r'(?:/{2}|#) *CHECK-NOT:\s*(.*)', line)
            if m:
                context.CheckNotList.append(m.group(1))
                continue

            m = re.search(r'(?:/{2}|#) *TEST-CASE: (.*) -> (.*)', line)
            if m:
                context.TestCases.append((m.group(1), m.group(2)))
                continue

            m = re.search(r'(?:/{2}|#) *COMPILE-FAIL', line)
            if m:
                context.NegativeTest = True
                continue

            m = re.search(r'(?:/{2}|#) *EXTRA-FLAGS: (.*)', line)
            if m:
                context.ExtraCompileFlags = m.group(1)
                continue

    return context


def linkFiles(context: Context):
    # Create the main.c file which used for the testsing
    CMainTemplate = "#include <stdio.h>\n\n"
    for funcDecl in context.FunctionDeclarations:
        CMainTemplate += funcDecl + ";\n"

    printLine = r'printf("\nExpected: %d, Actual: %d\n", @, res);'
    MainFunction = """
int main()
{
    int res = $;

    if(res != @)
    {
        %
        return 1;
    }

    return 0;
}
    """
    MainFunction = MainFunction.replace("%", printLine)
    CMainTemplate += MainFunction

    for case, expectedResult in context.TestCases:
        currentTestMain = CMainTemplate
        currentTestMain = currentTestMain.replace("$", case)
        currentTestMain = currentTestMain.replace("@", expectedResult)

        creatFiles("testMain.c", currentTestMain)

        # compile and link the C file with the generated assembly
        LinkCommand = [
            context.Arch + "-linux-gnu-gcc",
            "testMain.c",
            "test.s",
            "-o",
            "test",
            "-static",
            "-lm"
        ]

        compileRet = subprocess.run(LinkCommand).returncode
        if compileRet != 0:
            cleanCacheFiles()
            return False

        ret = subprocess.run([context.RunCommand, "test"], capture_output=True)
        if len(ret.stdout.decode()):
            print(ret.stdout.decode())
        if ret.returncode != 0 and not context.RunFailTest:
            print(ret.stderr.decode())
            return False

        return True


def executeTests(fileName, context: Context):
    if (context.RunTest or context.RunFailTest) and (context.Arch == "" or len(context.FunctionDeclarations) == 0 or len(context.TestCases) == 0):
        print(
            "run test file required specify arch name, function declaration and test cases")
        return False

    if context.CompileTest and len(context.CheckList) == 0 and len(context.CheckNotList) == 0:
        print("not CHECK were given")
        return False

    # Create the full command to call the LunarTcc Compiler
    command = [CompilerPath, fileName]
    if context.ExtraCompileFlags != "":
        command.extend(context.ExtraCompileFlags.split())

    # run the compile process
    result = subprocess.run(command, capture_output=True, timeout=10)

    # if the compilation failed
    if result.returncode != 0:
        # if it was a negative test and the fail did not caused by an assertion -> test passed
        isAssertion = re.search(r'(.*): Assertion(.*)', result.stderr.decode())
        if context.NegativeTest and not isAssertion:
            return True

        return False

    # if it was a negative test and did not failed then the test failed
    if context.NegativeTest:
        return False

    # if its a compile test then check the output
    if context.CompileTest:
        hadChecks = len(context.CheckList) > 0
        hadChecknots = len(context.CheckNotList) > 0

        for line in result.stdout.decode().splitlines():
            if hadChecks:
                if line.find(context.CheckList[0]) != -1:
                    context.CheckList.pop(0)

                    # found all check -> success
                    if len(context.CheckList) == 0:
                        return True

            if hadChecknots:
                # then check each CHECK-NOT
                for check_not in context.CheckNotList:
                    # if found one in the output -> fail
                    if line.find(check_not) != -1:
                        print("Output contains '", check_not,
                              "', but it should not")
                        return False
         # reached this point and had CHECKs -> did not found all check
        # print the next check which was not found
        if hadChecks:
            print("Have not found in the output: ", context.CheckList[0])
            return False

        # reached this point with CHECK-NOTS -> success
        if hadChecknots:
            return True

    # Run test cases
    # Create the assembly file
    creatFiles("test.s", result.stdout.decode())
    return linkFiles(context)


def HandleCommandLineArgs():
    for i, arg in enumerate(sys.argv):
        if i == 0:
            continue
        testSet.append(arg)


HandleCommandLineArgs()

if len(testSet) == 0:
    for subDir, dirs, files in os.walk(WorkDir):
        for fileName in files:
            filePath = subDir + os.sep + fileName

            if filePath.endswith(".c") or filePath.endswith(".s"):
                testSet.append(filePath)

testSet.sort()

# run the tests
for filePath in testSet:
    simplifyFilePath = filePath.replace(WorkDir, "")
    prettyFilePath = "[orange]" + simplifyFilePath + "[/orange]"
    context = checkFile(filePath)

    # if not test types were specified, then skip it
    if not context.RunFailTest and not context.RunTest and not context.CompileTest and not context.NegativeTest:
        continue

    success = executeTests(filePath, context)

    testsCount += 1
    if success:
        passedTestsCount += 1
        console.print("[green u]PASS[/green u]  " + prettyFilePath)
    else:
        failedTests.append(simplifyFilePath)
        console.print("[red u]FAIL[/red u]  " + prettyFilePath)

cleanCacheFiles()
print("\n--------", testsCount, "Test executed --------")
console.print("|\t", passedTestsCount, "   [green]PASS[/green]", "\t\t|")
console.print("|\t", len(failedTests), "   [red]FAIL[/red]", "\t\t|")
console.print("---------------------------------\n")

style = "bold yellow"
if len(failedTests) > 0:
    print("Failed:")

for case in failedTests:
    console.print("   ", case, style=style)
