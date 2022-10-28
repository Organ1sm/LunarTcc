import os
import re
import subprocess
from rich.console import Console


WorkDir = os.path.dirname(__file__)
CompilerPath = os.path.dirname(WorkDir) + "/bin/LunarTcc"
RunCommand = "qemu-"
console = Console()


def CreateFile(fileName, context):
    textFile = open(fileName, "w")
    n = textFile.write(context)
    textFile.close()


def CleanTestCacheFile():
    if os.path.exists("testMain.c"):
        os.remove("testMain.c")
    if os.path.exists("test.s"):
        os.remove("test.s")
    if os.path.exists("test"):
        os.remove("test")


def CheckAndCompileFile(fileName):
    Arch = ""
    FunctionDecls = []
    TestCases = []

    with open(fileName) as file:
        for line in file:
            m = re.search(r"(?:/{2}|#) *RUN: (.*)", line)
            if m:
                Arch = m.group(1).lower()
                continue

            m = re.search(r"(?:/{2}|#) *FUNC-DECL: (.*)", line)
            if m:
                FunctionDecls.append(m.group(1))
                continue

            m = re.search(r"(?:/{2}|#) *TEST-CASE: (.*) -> (\d+)", line)
            if m:
                TestCases.append((m.group(1), m.group(2)))
                continue

    if len(TestCases) == 0:
        return False, False

    if len(Arch) == 0:
        return False, True

    testMain_C_TemPlate = ""
    for funcDecl in FunctionDecls:
        testMain_C_TemPlate += funcDecl + ";\n"

    testMain_C_TemPlate += "int main() { return $; }"

    commandList = [CompilerPath, fileName]
    retCode = subprocess.run(commandList, stdout=subprocess.DEVNULL).returncode

    if retCode != 0:
        return False, True

    testAsm = subprocess.check_output(commandList).decode("utf-8")
    CreateFile("test.s", testAsm)

    for case, expectedResult in TestCases:
        currentTestMain = testMain_C_TemPlate
        currentTestMain = currentTestMain.replace("$", case + " == " + expectedResult)

        CreateFile("testMain.c", currentTestMain)

        LinkCommandList = [
            Arch + "-linux-gnu-gcc",
            "testMain.c",
            "test.s",
            "-o",
            "test",
            "-static",
        ]
        compileRet = subprocess.run(LinkCommandList).returncode
        if compileRet != 0:
            return False, True

        retCode = subprocess.run([RunCommand + Arch, "test"]).returncode
        if retCode != 1:
            return False, True

        return True, True


failedTests = []
testsCount = 0
passedTestsCount = 0

for subDir, dirs, files in os.walk(WorkDir):
    for fileName in files:
        filePath = subDir + os.sep + fileName
        simplifyFilePath = filePath.replace(WorkDir, "")
        prettyFilePath = "[orange]" + simplifyFilePath + "[/orange]"

        if filePath.endswith(".c") or filePath.endswith(".s"):
            success, hasCases = CheckAndCompileFile(filePath)

            if not hasCases:
                continue

            testsCount += 1
            if success:
                passedTestsCount += 1
                console.print(prettyFilePath + "  [green u]PASS[/green u]")
            else:
                failedTests.append(simplifyFilePath)
                console.print(prettyFilePath + " [red u]FAILED[/red u]")

CleanTestCacheFile()
print("\n--------", testsCount, "Test executed --------")
console.print("|\t", passedTestsCount, "   [green]PASS[/green]", "\t\t|")
console.print("|\t", len(failedTests), "   [red]FAIL[/red]", "\t\t|")
console.print("---------------------------------\n")

style = "bold yellow"
if len(failedTests) > 0:
    print("Failed:")

for case in failedTests:
    console.print("   ", case, style=style)
