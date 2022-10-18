#include "FrontEnd/Parser/Parser.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"

bool Parser::IsTypeSpecifier(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::Char:
        case Token::Int:
        case Token::Double:
        case Token::Struct:
            return true;

        default:
            break;
    }

    return false;
}

static bool IsUnaryOperator(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::Mul:
            return true;

        default:
            break;
    }

    return false;
}

bool Parser::IsReturnTypeSpecifier(Token::TokenKind tk)
{
    return tk == Token::Void || IsTypeSpecifier(tk);
}

Type Parser::ParseType(Token::TokenKind tk)
{
    Type Result;
    switch (tk)
    {
        case Token::Void:
            Result.SetTypeVariant(Type::Void);
            break;
        case Token::Char:
            Result.SetTypeVariant(Type::Char);
            break;
        case Token::Int:
            Result.SetTypeVariant(Type::Int);
            break;
        case Token::Double:
            Result.SetTypeVariant(Type::Double);
            break;
        case Token::Struct:
            {
                Lex();    // eat 'struct'

                auto CurrentTK   = lexer.GetCurrentToken();
                std::string Name = CurrentTK.GetString();

                Result = std::get<0>(UserDefinedTypes[Name]);
                break;
            }
        default:
            assert(!"Unknown token kind.");
            break;
    }

    return Result;
}

static void UndefinedSymbolError(Token sym, Lexer &L)
{
    std::cout << ":" << sym.GetLine() + 1 << ":" << sym.GetColumn() + 1 << ": error: "
              << "Undefined symbol '" << sym.GetString() << "'." << std::endl
              << "\t\t" << L.GetSource()[sym.GetLine()].substr(sym.GetColumn())
              << std::endl
              << std::endl;
}

static void ArrayTypeMismatchError(Token Sym, Type Actual)
{
    std::cout << Sym.GetLine() + 1 << ":" << Sym.GetColumn() + 1 << " error: "
              << " FuncType mismatch `" << Sym.GetString() << "` type is `"
              << Actual.ToString() << "`, it is not an array type." << std::endl;
}

void static EmitError(const std::string &msg, Lexer &L)
{
    std::cout << ":" << L.GetLine() << ": error: " << msg << std::endl
              << "\t\t" << L.GetSource()[L.GetLine() - 1] << std::endl
              << std::endl;
}

void static EmitError(const std::string &msg, Lexer &L, Token &T)
{
    std::cout << ":" << T.GetLine() + 1 << ":" << T.GetColumn() + 1 << ": error: " << msg
              << std::endl
              << "\t\t" << L.GetSource()[T.GetLine()] << std::endl
              << std::endl;
}

Token Parser::Expect(Token::TokenKind TKind)
{
    auto t = Lex();

    if (t.GetKind() != TKind)
    {
        std::cout << ":" << t.GetLine() + 1 << ":" << t.GetColumn() + 1
                  << ": error: Unexpected symbol `" << t.GetString() << "`. Expected is `"
                  << Token::ToString(TKind) << "`."
                  << "\t\t" << lexer.GetSource()[t.GetLine()] << std::endl
                  << std::endl;
    }

    return t;
}

std::unique_ptr<Node> Parser::Parse() { return ParseExternalDeclaration(); }

unsigned Parser::ParseIntegerConstant()
{
    Token T         = Expect(Token::Integer);
    auto TokenStr   = T.GetString();
    unsigned Result = 0;

    for (auto c : TokenStr)
    {
        Result *= 10;
        Result += c - '0';
    }

    return Result;
}

double Parser::ParseRealConstant()
{
    Token T          = Expect(Token::Real);
    auto TokenStr    = T.GetString();
    double WholePart = 0.0;

    for (auto c : TokenStr)
    {
        if (c == '.')
            break;

        WholePart *= 10;
        WholePart += (c - '0');
    }

    double FractionalPart = 0.0;
    unsigned Divider      = 1;

    for (auto c : TokenStr.substr(TokenStr.find('.') + 1))
    {
        FractionalPart += (c - '0');
        Divider *= 10;
    }

    FractionalPart /= Divider;

    return WholePart + FractionalPart;
}

std::unique_ptr<Node> Parser::ParseTranslationUnit() { return std::unique_ptr<Node>(); }


//=--------------------------------------------------------------------------=//
//=------------------------- Parse Declaration ------------------------------=//
//=--------------------------------------------------------------------------=//

// <ExternalDeclaration> ::= <FunctionDeclaration>
//                         | <VariableDeclaration>
std::unique_ptr<Node> Parser::ParseExternalDeclaration()
{
    std::unique_ptr<TranslationUnit> TU = std::make_unique<TranslationUnit>();
    auto TokenKind                      = GetCurrentTokenKind();

    while (IsReturnTypeSpecifier(TokenKind) || lexer.Is(Token::Struct))
    {
        if (lexer.Is(Token::Struct))
        {
            TU->AddDeclaration(ParseStructDeclaration());
            TokenKind = GetCurrentTokenKind();
            continue;
        }

        Type Ty            = ParseType(TokenKind);
        CurrentFuncRetType = Ty;
        Lex();

        auto Name    = Expect(Token::Identifier);
        auto NameStr = Name.GetString();

        // typeSpecifier funcName (T1 a, ...);
        if (lexer.Is(Token::LeftParen))
        {
            TU->AddDeclaration(ParseFunctionDeclaration(Ty, Name));
        }
        else
        {
            // Variable Declaration;
            // int a = 0;
            std::vector<unsigned> Dimensions;

            // Array Declaration.
            while (lexer.Is(Token::LeftBracket))
            {
                Lex();    // consume '['
                Dimensions.push_back(ParseIntegerConstant());
                Expect(Token::RightBracket);
                if (lexer.IsNot(Token::Comma))
                    break;
                Lex();    // consume ','
            }

            Expect(Token::SemiColon);

            InsertToSymbolTable(NameStr, Type(Ty, Dimensions));

            TU->AddDeclaration(
                std::make_unique<VariableDeclaration>(NameStr, Ty, Dimensions));
        }

        TokenKind = GetCurrentTokenKind();
    }

    return TU;
}
/// <FunctionDeclaration> := <ReturnTypeSpecifier> <Identifier>
///                             '(' <ParameterList>? ')' ';'
///                        | <FunctionDefinition>
/// <FunctionDefinition> :=  <ReturnTypeSpecifier> <Identifier>
///                             '(' <ParameterList>? ')' <CompoundStatement>
///
std::unique_ptr<FunctionDeclaration>
    Parser::ParseFunctionDeclaration(const Type &ReturnType, const Token &Name)
{
    Expect(Token::LeftParen);

    // Creating new scope by pushing a new symbol table to the stack.
    SymTabStack.PushSymbolTable();
    auto PL = ParseParameterList();

    Expect(Token::RightParen);

    auto FuncType = FunctionDeclaration::CreateType(ReturnType, PL);
    auto NameStr  = Name.GetString();
    InsertToSymbolTable(NameStr, Type(FuncType), true);

    /// Assume function is defined
    // TODO: planed to have function declaration.
    auto Body = ParseCompoundStatement();

    SymTabStack.PopSymbolTable();

    return std::make_unique<FunctionDeclaration>(FuncType, NameStr, PL, Body);
}

// <ParameterDeclaration> ::= { <TypeSpecifier> '*' <Identifier>? }?
std::unique_ptr<FunctionParameterDeclaration> Parser::ParseParameterDeclaration()
{
    std::unique_ptr<FunctionParameterDeclaration> FPD =
        std::make_unique<FunctionParameterDeclaration>();

    if (IsTypeSpecifier(GetCurrentTokenKind()))
    {
        Type Ty = ParseTypeSpecifier();
        Lex();

        while (lexer.Is(Token::Mul))
        {
            Ty.IncrementPointerLevel();
            Lex();    // Eat the `*` character
        }

        FPD->SetType(Ty);

        if (lexer.Is(Token::Identifier))
        {
            auto IdName = Lex().GetString();
            FPD->SetName(IdName);
            InsertToSymbolTable(IdName, Ty);
        }
    }

    return FPD;
}

// <ParameterList> ::= <ParameterDeclaration>? {',' <ParameterDeclaration>}*
std::vector<std::unique_ptr<FunctionParameterDeclaration>> Parser::ParseParameterList()
{
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> Params;

    if (!IsTypeSpecifier(GetCurrentTokenKind()))
        return Params;

    Params.push_back(ParseParameterDeclaration());

    while (lexer.Is(Token::Comma))
    {
        Lex();
        Params.push_back(ParseParameterDeclaration());
    }

    return Params;
}


Type Parser::ParseTypeSpecifier()
{
    auto TokenKind = GetCurrentTokenKind();

    if (!IsTypeSpecifier(TokenKind))
        assert(!"Invalid type");

    return ParseType(TokenKind);
}

Node Parser::ParseReturnTypeSpecifier() { return Node(); }


// <VaraibleDeclaration> ::= <TypeSpecifier> <Identifier>
//                           {'[' <IntegerConstant> ]'}* ';'
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDeclaration()
{
    auto Ty = ParseTypeSpecifier();
    Lex();

    while (lexer.Is(Token::Mul))
    {
        Ty.IncrementPointerLevel();
        Lex();    // Eat the `*` character
    }

    std::string Name = Expect(Token::Identifier).GetString();
    std::vector<unsigned> Dimensions;

    while (lexer.Is(Token::LeftBracket))
    {
        Lex();
        Dimensions.push_back(ParseIntegerConstant());
        Expect(Token::RightBracket);
    }

    if (!Dimensions.empty())
        Ty = Type(Ty, Dimensions);

    Expect(Token::SemiColon);

    InsertToSymbolTable(Name, Ty);

    return std::make_unique<VariableDeclaration>(Name, Ty, Dimensions);
}

std::unique_ptr<MemberDeclaration> Parser::ParseMemberDeclaration()
{
    Type Ty = ParseTypeSpecifier();
    Lex();

    while (lexer.Is(Token::Mul))
    {
        Ty.IncrementPointerLevel();
        Lex();    // Eat the * character
    }

    std::string Name = Expect(Token::Identifier).GetString();

    std::vector<unsigned> Dimensions;
    while (lexer.Is(Token::LeftBracket))
    {
        Lex();
        Dimensions.push_back(ParseIntegerConstant());
        Expect(Token::RightBracket);
    }

    Expect(Token::SemiColon);

    return std::make_unique<MemberDeclaration>(Name, Ty, Dimensions);
}

// <StructDeclaration> ::= 'struct' <Identifier>
//                                  '{' <StructDeclarationList>+ '}' ';'
std::unique_ptr<StructDeclaration> Parser::ParseStructDeclaration()
{
    Expect(Token::Struct);

    std::string Name = Expect(Token::Identifier).GetString();

    Expect(Token::LeftBrace);    // eat '{'

    std::vector<std::unique_ptr<MemberDeclaration>> Members;
    Type type(Type::Struct);
    type.SetName(Name);

    std::vector<std::string> StructMemberIdentifiers;
    while (lexer.IsNot(Token::RightBrace))
    {
        auto MD = ParseMemberDeclaration();
        type.GetTypeList().push_back(MD->GetType());
        StructMemberIdentifiers.push_back(MD->GetName());
        Members.push_back(std::move(MD));
    }

    Expect(Token::RightBrace);
    Expect(Token::SemiColon);

    // saving the struct type and name
    UserDefinedTypes[Name] = {type, std::move(StructMemberIdentifiers)};

    return std::make_unique<StructDeclaration>(Name, Members, type);
}

//=--------------------------------------------------------------------------=//
//=------------------------- Parse Statement --------------------------------=//
//=--------------------------------------------------------------------------=//

// <Statement> ::= <ExpressionStatement>
//               | <WhileStatement>
//               | <IfStatement>
//               | <ForStatement>
//               | <CompoundStatement>
//               | <ReturnStatement>
std::unique_ptr<Statement> Parser::ParseStatement()
{
    if (lexer.Is(Token::If))
        return ParseIfStatement();
    if (lexer.Is(Token::While))
        return ParseWhileStatement();
    if (lexer.Is(Token::For))
        return ParseForStatement();
    if (lexer.Is(Token::LeftBrace))
        return ParseCompoundStatement();
    if (lexer.Is(Token::Return))
        return ParseReturnStatement();

    return ParseExpressionStatement();
}
// <IfStatement> ::= if '(' <Expression> ')' <Statement> {else <Statement>}?
std::unique_ptr<IfStatement> Parser::ParseIfStatement()
{
    auto IS = std::make_unique<IfStatement>();

    Expect(Token::If);
    Expect(Token::LeftParen);
    IS->SetCondition(std::move(ParseExpression()));
    Expect(Token::RightParen);
    IS->SetIfBody(std::move(ParseStatement()));

    if (lexer.Is(Token::Else))
    {
        Lex();
        IS->SetElseBody(ParseStatement());
    }

    return IS;
}

// <WhileStatement> ::= while '(' <Expression> ')' <Statement>
std::unique_ptr<WhileStatement> Parser::ParseWhileStatement()
{
    auto WS = std::make_unique<WhileStatement>();
    Expect(Token::While);
    Expect(Token::LeftParen);
    WS->SetCondition(std::move(ParseExpression()));
    Expect(Token::RightParen);
    WS->SetBody(std::move(ParseStatement()));

    return WS;
}

// <ForStatement> ::= for '(' <Expression> ')' <Statement>
std::unique_ptr<ForStatement> Parser::ParseForStatement()
{
    std::unique_ptr<ForStatement> FS = std::make_unique<ForStatement>();

    Expect(Token::For);
    Expect(Token::LeftParen);

    FS->SetInit(std::move(ParseExpression()));
    Expect(Token::SemiColon);

    FS->SetCondition(std::move(ParseExpression()));
    Expect(Token::SemiColon);

    FS->SetIncrement(std::move(ParseExpression()));
    Expect(Token::RightParen);

    FS->SetBody(std::move(ParseStatement()));

    return FS;
}


// <ReturnStatement> ::= return <Expression>? ';'
// TODO: we need Explicit type conversioins here as well
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement()
{
    Expect(Token::Return);

    auto Expr      = ParseExpression();
    auto LeftType  = CurrentFuncRetType.GetTypeVariant();
    auto RightType = Expr->GetResultType().GetTypeVariant();

    std::unique_ptr<ReturnStatement> RS;

    if (LeftType != RightType)
    {
        std::unique_ptr<Expression> CastExpr =
            std::make_unique<ImplicitCastExpression>(std::move(Expr), LeftType);

        RS = std::make_unique<ReturnStatement>(std::move(CastExpr));
    }
    else
    {
        RS = std::make_unique<ReturnStatement>(std::move(Expr));
    }

    Expect(Token::SemiColon);

    return RS;
}

// <CompoundStatement> ::= '{' <VariableDeclaration>* <Statement>* '}'
std::unique_ptr<CompoundStatement> Parser::ParseCompoundStatement()
{
    Expect(Token::LeftBrace);

    std::vector<std::unique_ptr<VariableDeclaration>> Declarations;
    std::vector<std::unique_ptr<Statement>> Statements;

    while (IsTypeSpecifier(GetCurrentTokenKind()))
        Declarations.push_back(std::move(ParseVariableDeclaration()));

    while (lexer.IsNot(Token::RightBrace))
        Statements.push_back(std::move(ParseStatement()));

    Expect(Token::RightBrace);

    return std::make_unique<CompoundStatement>(Declarations, Statements);
}

// <ExpressionStatement> ::= <Expression>? ';'
std::unique_ptr<ExpressionStatement> Parser::ParseExpressionStatement()
{
    auto ES = std::make_unique<ExpressionStatement>();

    if (lexer.IsNot(Token::SemiColon))
        ES->SetExpression(std::move(ParseExpression()));
    Expect(Token::SemiColon);

    return ES;
}

//=--------------------------------------------------------------------------=//
//=------------------------- Parse Expression -------------------------------=//
//=--------------------------------------------------------------------------=//

std::unique_ptr<Expression> Parser::ParseExpression() { return ParseBinaryExpression(); }

// <PostFixExpression> ::= <PrimaryExpression>
//                       | <PostFixExpression> '(' <Arguments> ')'
//                       | <PostFixExpression> '[' <Expression> ']'
//                       | <PostFixExpression> '.' <Identifier>
std::unique_ptr<Expression> Parser::ParsePostFixExpression()
{
    auto CurrentToken = lexer.GetCurrentToken();
    auto Expr         = ParsePrimaryExpression();
    assert(Expr && "Cannot be NULL");

    while (lexer.Is(Token::LeftParen) || lexer.Is(Token::LeftBracket)
           || lexer.Is(Token::Dot))
    {
        // Parse a CallExpression here
        if (lexer.Is(Token::LeftParen))
        {
            Expr = ParseCallExpression(CurrentToken);
        }
        // parse ArrayExpression
        else if (lexer.Is(Token::LeftBracket))
        {
            Expr = ParseArrayExpression(std::move(Expr));
        }
        // parse StructMemberAccess
        else if (lexer.Is(Token::Dot))
        {
            Lex();    // eat the Dot
            auto MemberId    = Expect(Token::Identifier);
            auto MemberIdStr = MemberId.GetString();

            assert(Expr->GetResultType().IsStruct() && "TODO: emit error");
            // find the type of the member
            auto StructDataTuple   = UserDefinedTypes[Expr->GetResultType().GetName()];
            auto StructType        = std::get<0>(StructDataTuple);
            auto StructMemberNames = std::get<1>(StructDataTuple);

            size_t i = 0;
            for (; i < StructMemberNames.size(); i++)
                if (StructMemberNames[i] == MemberIdStr)
                    break;

            assert(i <= StructMemberNames.size() && "Member not found");

            Expr =
                std::make_unique<StructMemberReference>(std::move(Expr), MemberIdStr, i);
        }
    }

    return Expr;
}

static int GetBinOpPrecedence(Token::TokenKind TK)
{
    switch (TK)
    {
        case Token::Assign:
            return 10;
        case Token::LogicalAnd:
            return 20;
        case Token::And:
            return 30;
        case Token::Equal:
        case Token::NotEqual:
            return 40;
        case Token::Less:
        case Token::Greater:
            return 50;
        case Token::Plus:
        case Token::Minus:
            return 60;
        case Token::Mul:
        case Token::Div:
        case Token::Mod:
            return 70;
        case Token::Not:
            return 80;
        default:
            return -1;
    }
}

std::unique_ptr<Expression> Parser::ParseUnaryExpression()
{
    auto UnaryOperation = lexer.GetCurrentToken();

    if (!IsUnaryOperator(UnaryOperation.GetKind()))
        return ParsePostFixExpression();

    Lex();

    std::unique_ptr<Expression> Expr;

    if (IsUnaryOperator(GetCurrentTokenKind()))
        return std::make_unique<UnaryExpression>(UnaryOperation,
                                                 std::move(ParseUnaryExpression()));

    // TODO: Add semantic check that only pointer types are dereferenced
    Expr = ParsePostFixExpression();

    return std::make_unique<UnaryExpression>(UnaryOperation, std::move(Expr));
}

std::unique_ptr<Expression> Parser::ParseBinaryExpression()
{
    auto LeftExpression = ParseUnaryExpression();
    assert(LeftExpression && "Cannot be Null");

    return ParseBinaryExpressionRHS(0, std::move(LeftExpression));
}

// <PrimaryExpression> ::= <IdentifierExpression>
//                       | '(' <Expression> ')'
//                       | <ConstantExpression>
std::unique_ptr<Expression> Parser::ParsePrimaryExpression()
{
    if (lexer.Is(Token::LeftParen))
    {
        Lex();    // consume '('
        auto Expression = ParseExpression();
        Expect(Token::RightParen);

        return Expression;
    }
    else if (lexer.Is(Token::Identifier))
        return ParseIdentifierExpression();
    else if (lexer.Is(Token::Real) || lexer.Is(Token::Integer))
        return ParseConstantExpression();
    else
        return nullptr;
}

// <IdentifierExpression> ::= CallExpression
//                          | Identifier {'[' <Expression> ']'}+
std::unique_ptr<Expression> Parser::ParseIdentifierExpression()
{
    auto Id = Expect(Token::Identifier);

    auto RE    = std::make_unique<ReferenceExpression>(Id);
    auto IdStr = Id.GetString();

    if (auto SymEntry = SymTabStack.Contains(IdStr))
    {
        auto Ty = std::get<1>(SymEntry.value());
        RE->SetResultType(Ty);
    }
    else if (UserDefinedTypes.count(IdStr) > 0)
    {
        auto Ty = std::get<0>(UserDefinedTypes[IdStr]);
        RE->SetResultType(std::move(Ty));
    }
    else
        UndefinedSymbolError(Id, lexer);

    return RE;
}

std::unique_ptr<Expression> Parser::ParseCallExpression(Token Id)
{
    // FIXME: Make this a semantic check
    assert(Id.GetKind() == Token::Identifier && "Identifier expected");
    Lex();    // eat the '('

    Type FuncType;

    if (auto SymEntry = SymTabStack.Contains(Id.GetString()))
        FuncType = std::get<1>(SymEntry.value());
    else
        UndefinedSymbolError(Id, lexer);

    std::vector<std::unique_ptr<Expression>> CallArgs;

    if (lexer.IsNot(Token::RightParen))
        CallArgs.push_back(ParseExpression());

    while (lexer.Is(Token::Comma))
    {
        Lex();
        CallArgs.push_back(ParseExpression());
    }

    // Currently a function without argument is actually a function with
    // a type of ...(void), which is a special case checked first.
    auto FuncArgTypes = FuncType.GetArgTypes();
    auto FuncArgNum   = FuncArgTypes.size();
    if (!(CallArgs.size() == 0 && FuncArgNum == 1 && FuncArgTypes[0] == Type::Void))
    {
        if (FuncArgNum != CallArgs.size())
            EmitError("arguments number mismatch", lexer);

        for (size_t i = 0; i < FuncArgNum; i++)
        {
            auto CallArgType = CallArgs[i]->GetResultType().GetTypeVariant();

            // If the ith argument type is not matching the expeccted one
            if (CallArgType != FuncArgTypes[i].GetTypeVariant())
            {
                // Cast if allowed
                if (Type::IsImplicitlyCastable(CallArgType,
                                               FuncArgTypes[i].GetTypeVariant()))
                    CallArgs[i] = std::make_unique<ImplicitCastExpression>(
                        std::move(CallArgs[i]), FuncArgTypes[i]);
                else    // otherwise its an error
                    EmitError("argument type mismatch", lexer);
            }
        }
    }

    Expect(Token::RightParen);

    return std::make_unique<CallExpression>(Id.GetString(), CallArgs, FuncType);
}

std::unique_ptr<Expression> Parser::ParseArrayExpression(std::unique_ptr<Expression> Base)
{
    Lex();
    auto IndexExpr = ParseExpression();
    Expect(Token::RightBracket);

    Type ActualType;

    // Type type = std::move(ActualType);
    Type type = Base->GetResultType();

    /// Remove the first N dimensions from the actual type. Example:
    /// ActualType is 'int arr[5][10]' and our reference is 'arr[0]'
    /// then the result type of 'arr[0]' is 'int[10]'. N is the
    /// amount of index expressions used when referencing the array here
    /// 'arr'. In the example its 1.
    type.GetDimensions().erase(type.GetDimensions().begin(),
                               type.GetDimensions().begin() + 1);

    Base->SetLValueness(true);
    return std::make_unique<ArrayExpression>(Base, IndexExpr, type);
}

// <ConstantExpression> ::= [1-9][0-9]*
//                        | [0-9]+.[0-9]+
std::unique_ptr<Expression> Parser::ParseConstantExpression()
{
    if (lexer.Is(Token::Integer))
        return std::make_unique<IntegerLiteralExpression>(ParseIntegerConstant());
    else
        return std::make_unique<FloatLiteralExpression>(ParseRealConstant());
}

std::unique_ptr<Expression>
    Parser::ParseBinaryExpressionRHS(int Precedence,
                                     std::unique_ptr<Expression> LeftExpression)
{
    while (true)
    {
        int TokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        if (TokenPrec < Precedence)
            return LeftExpression;

        Token BinaryOperator = Lex();

        auto RightExpression = ParseUnaryExpression();

        /// In case of Assignment check if the left operand since if should be an
        /// lvalue. Which is either an identifier reference or an array expression.
        if (BinaryOperator.GetKind() == Token::Assign
            && !dynamic_cast<ReferenceExpression *>(LeftExpression.get())
            && !dynamic_cast<ArrayExpression *>(LeftExpression.get())
            && !dynamic_cast<StructMemberReference *>(LeftExpression.get()))
        {
            EmitError("lvalue required as left operand of assignment", lexer,
                      BinaryOperator);
        }

        /// If it's an Assign BinaryOperator and the left hand side is an ArrayExpression
        /// or ReferenceExpression, then it's an LValue.
        /// This can reduces one time load instruction generate for global Variable.

        /// Fixme: Should be solved in a better ways. Seems like LLVM using
        /// ImplicitCastExpression for this purpose as well.
        if (BinaryOperator.GetKind() == Token::Assign)
        {
            if (auto LE = dynamic_cast<ArrayExpression *>(LeftExpression.get()))
                LE->SetLValueness(true);
            else if (auto LE = dynamic_cast<ReferenceExpression *>(LeftExpression.get()))
                LE->SetLValueness(true);
            else if (auto LE =
                         dynamic_cast<StructMemberReference *>(LeftExpression.get()))
                LE->SetLValueness(true);
        }

        int NextTokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        int Associviaty = 1;    // left associative
        if (BinaryOperator.GetKind() == Token::Assign)
        {
            Associviaty = 0;    // right associative
            NextTokenPrec++;
        }

        if (TokenPrec < NextTokenPrec)
            RightExpression = ParseBinaryExpressionRHS(TokenPrec + Associviaty,
                                                       std::move(RightExpression));

        // Implicit Cast Insertion if needed.
        auto LeftType  = LeftExpression->GetResultType().GetTypeVariant();
        auto RightType = RightExpression->GetResultType().GetTypeVariant();

        if (LeftType != RightType)
        {
            /// if an assignment, then try to cast the RHS to type of LHS.
            if (BinaryOperator.GetKind() == Token::Assign)
            {
                if (!Type::IsImplicitlyCastable(RightType, LeftType))
                    EmitError("FuncType mismatch", lexer, BinaryOperator);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression), LeftType);
            }
            /// Otherwise cast the one with lower conversion rank to higher one .
            else
            {
                auto DesiredType =
                    Type::GetStrongestType(LeftType, RightType).GetTypeVariant();

                // if LHS needs the conversion.
                if (LeftType != DesiredType)
                    LeftExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(LeftExpression), DesiredType);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression), DesiredType);
            }
        }
        /// mod operation
        else if (BinaryOperator.GetKind() == Token::Mod)
        {
            if (LeftType != Type::Int || RightType != Type::Int)
                EmitError("Mod Operator can only operator on integers", lexer,
                          BinaryOperator);
        }

        LeftExpression = std::make_unique<BinaryExpression>(
            std::move(LeftExpression), BinaryOperator, std::move(RightExpression));
    }
}

// TODO: To report the location of error we would need the token holding the
// symbol name. It would be wise anyway to save it in AST nodes rather than
// just a string.
void Parser::InsertToSymbolTable(const std::string &SymbolName,
                                 Type SymType,
                                 const bool ToGlobal,
                                 ValueType SymValue)
{
    SymbolTableStack::Entry SymEntry(SymbolName, SymType, SymValue);
    if (SymTabStack.ContainsInCurrentScope(SymEntry))
    {
        std::cout << "Error: Symbol '" + SymbolName + "' with type '" + SymType.ToString()
                         + "' is already defined."
                  << std::endl
                  << std::endl;
    }
    else
    {
        ToGlobal ? SymTabStack.InsertGlobalEntry(SymEntry) :
                   SymTabStack.InsertEntry(SymEntry);
    }
}
