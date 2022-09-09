#include "frontend/Parser/Parser.hpp"

static bool IsTypeSpecifier(Token::TokenKind tk)
{
    return tk == Token::Int || tk == Token::Double;
}

static bool IsReturnTypeSpecifier(Token::TokenKind tk)
{
    return tk == Token::Void || IsTypeSpecifier(tk);
}

static Type ParseType(Token::TokenKind tk)
{
    Type Result;
    switch (tk)
    {
        case Token::Void:
            Result.SetTypeVariant(Type::Void);
            break;
        case Token::Int:
            Result.SetTypeVariant(Type::Int);
            break;
        case Token::Double:
            Result.SetTypeVariant(Type::Double);
            break;
        default:
            // TODO: emit error
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

static void ArrayTypeMismatchError(Token Sym, ComplexType Actual)
{
    std::cout << Sym.GetLine() + 1 << ":" << Sym.GetColumn() + 1 << " error: "
              << " Type mismatch `" << Sym.GetString() << "` type is `"
              << Actual.ToString() << "`, it is not an array type." << std::endl;
}

void static EmitErrorWithLineInfoAndAffectedLine(const std::string &msg, Lexer &L)
{
    std::cout << ":" << L.GetLine() << ": error: " << msg << std::endl
              << "\t\t" << L.GetSource()[L.GetLine() - 1] << std::endl
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

double Parser::ParseRealConstant() { return 0; }

std::unique_ptr<Node> Parser::ParseTranslationUnit() { return std::unique_ptr<Node>(); }


// <ExternalDeclaration> ::= <FunctionDeclaration>
//                         | <VaraibleDeclaration>
std::unique_ptr<Node> Parser::ParseExternalDeclaration()
{
    std::unique_ptr<TranslationUnit> TU = std::make_unique<TranslationUnit>();
    auto TokenKind                      = GetCurrentTokenKind();

    while (IsReturnTypeSpecifier(TokenKind))
    {
        Type Type = ParseType(TokenKind);
        Lex();

        auto Name    = Expect(Token::Identifier);
        auto NameStr = Name.GetString();

        // typeSpecifier funcName (T1 a, ...);
        if (lexer.Is(Token::LeftParen))
        {
            Lex();    // consume '('

            // Creating new scope by pushing a new symbol table to the stack.
            SymTabStack.PushSymbolTable();

            auto PL = ParseParameterList();

            Expect(Token::RightParen);

            // for now assume that a function is defined
            // TODO: planed to have function Declaration.
            auto Body = ParseCompoundStatement();

            auto Function =
                std::make_unique<FunctionDeclaration>(Type, NameStr, PL, Body);

            // Removing the function's scope since we're done with its parsing.
            SymTabStack.PopSymbolTable();

            // FIXME: If we add the function to the symbol table here then its mean we
            // cannot call it from within this function (recursive call) since it get
            // only defined after parsing the function
            InsertToSymbolTable(NameStr, ComplexType(Function->GetType()));

            TU->AddDeclaration(std::move(Function));
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

            InsertToSymbolTable(NameStr, ComplexType(Type, Dimensions));

            TU->AddDeclaration(
                std::make_unique<VariableDeclaration>(NameStr, Type, Dimensions));
        }

        TokenKind = GetCurrentTokenKind();
    }

    return TU;
}

std::unique_ptr<Node> Parser::ParseFunctionDeclaration()
{
    return std::unique_ptr<Node>();
}

// <ParameterDeclaration> ::= { <TypeSpecifier> <Identifier>? }?
std::unique_ptr<FunctionParameterDeclaration> Parser::ParseParameterDeclaration()
{
    std::unique_ptr<FunctionParameterDeclaration> FPD =
        std::make_unique<FunctionParameterDeclaration>();

    if (IsTypeSpecifier(GetCurrentTokenKind()))
    {
        Type type = ParseTypeSpecifier();
        Lex();
        FPD->SetType(type);

        if (lexer.Is(Token::Identifier))
        {
            auto IdName = Lex().GetString();
            FPD->SetName(IdName);
            InsertToSymbolTable(IdName, ComplexType(type));
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

    // TODO emit Error;
    if (!IsTypeSpecifier(TokenKind))
        ;

    return ParseType(TokenKind);
}

Node Parser::ParseReturnTypeSpecifier() { return Node(); }


// <VaraibleDeclaration> ::= <TypeSpecifier> <Identifier>
//                           {'[' <IntegerConstant> ]'}* ';'
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDeclaration()
{
    auto Type = ParseTypeSpecifier();
    Lex();

    std::string Name = Expect(Token::Identifier).GetString();
    std::vector<unsigned> Dimensions;

    while (lexer.Is(Token::LeftBracket))
    {
        Lex();
        Dimensions.push_back(ParseIntegerConstant());
        Expect(Token::RightBracket);
    }

    Expect(Token::SemiColon);

    InsertToSymbolTable(Name, ComplexType(Type, Dimensions));

    return std::make_unique<VariableDeclaration>(Name, Type, Dimensions);
}


// <Statement> ::= <ExpressionStatement>
//               | <WhileStatement>
//               | <IfStatement>
//               | <CompoundStatement>
//               | <ReturnStatement>
std::unique_ptr<Statement> Parser::ParseStatement()
{
    if (lexer.Is(Token::If))
        return ParseStatement();
    if (lexer.Is(Token::While))
        return ParseWhileStatement();
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

// <ReturnStatement> ::= return <Expression>? ';'
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement()
{
    Expect(Token::Return);
    auto RS = std::make_unique<ReturnStatement>(ParseExpression());
    Expect(Token::SemiColon);

    return RS;
}

// <CompoundStatement> ::= '{' <VaraibleDeclaration>* <Statement>* '}'
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

std::unique_ptr<Expression> Parser::ParseExpression() { return ParseBinaryExpression(); }

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

std::unique_ptr<Expression> Parser::ParseBinaryExpression()
{
    auto LeftExpression = ParsePrimaryExpression();
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
    else
        return ParseConstantExpression();
}

// <IdentifierExpression> ::= CallExpression
//                          | Identifier {'[' <Expression> ']'}+
std::unique_ptr<Expression> Parser::ParseIdentifierExpression()
{
    auto Id       = Expect(Token::Identifier);
    auto SymEntry = SymTabStack.Contains(Id.GetString());

    if (lexer.IsNot(Token::LeftParen) && lexer.IsNot(Token::LeftBracket))
    {
        auto RE = std::make_unique<ReferenceExpression>(Id);

        if (SymEntry != std::nullopt)
        {
            auto Type = std::get<1>(SymEntry.value());
            RE->SetResultType(Type);
        }
        else
        {
            UndefinedSymbolError(Id, lexer);
        }

        return RE;
    }

    // CallExpression
    if (lexer.Is(Token::LeftParen))
    {
        Lex();

        ComplexType ReturnType;
        std::vector<std::unique_ptr<Expression>> Args;

        if (SymEntry != std::nullopt)
        {
            ReturnType = std::get<1>(SymEntry.value()).GetFunctionType();
        }
        else
        {
            UndefinedSymbolError(Id, lexer);
        }

        if (lexer.IsNot(Token::RightParen))
            Args.push_back(ParseExpression());

        while (lexer.Is(Token::Comma))
        {
            Lex();
            Args.push_back(ParseExpression());
        }

        // Get The Function Parameter Type List.
        auto RetArgTypes = ReturnType.GetArgTypes();
        if (RetArgTypes.size() != Args.size())
            EmitErrorWithLineInfoAndAffectedLine("arguments number mismatch", lexer);

        // Checking if the given arguments has the expected types for the function.
        // If not and implicit cast  not allowed then emit error. If allowed then
        // insert ImplicitCastExpression node with a child of the problematic
        // expression into the AST.
        for (int i = 0; i < RetArgTypes.size(); i++)
        {
            if (Args[i]->GetResultType().GetTypeVariant() != RetArgTypes[i])
            {
                if (Type::IsImplicitlyCastable(Args[i]->GetResultType().GetTypeVariant(),
                                               RetArgTypes[i]))
                {
                    Args[i] = std::make_unique<ImplicitCastExpression>(std::move(Args[i]),
                                                                       RetArgTypes[i]);
                }
                else
                {
                    EmitErrorWithLineInfoAndAffectedLine("argument type mismatch", lexer);
                }
            }
        }

        Expect(Token::RightParen);
        return std::make_unique<CallExpression>(Id.GetString(), Args, ReturnType);
    }

    // ArrayExpression
    if (lexer.Is(Token::LeftBracket))
    {
        Lex();
        std::vector<std::unique_ptr<Expression>> IndexExpressions;
        IndexExpressions.push_back(ParseExpression());
        Expect(Token::RightBracket);

        while (lexer.Is(Token::LeftBracket))
        {
            Lex();
            IndexExpressions.push_back(ParseExpression());
            Expect(Token::RightBracket);
        }

        ComplexType Type;
        if (SymEntry)
        {
            ComplexType ActualType = std::get<1>(SymEntry.value());

            // We try to access to much dimension.
            // int arr[10] -> arr[1][2]
            if (!ActualType.IsArrayType()
                || (IndexExpressions.size() > ActualType.GetDimensions().size()))
                ArrayTypeMismatchError(Id, ActualType);

            Type = std::move(ActualType);


            /// Remove the first N dimensions fromt the actual type.
            /// Example:
            /// ActualType is 'int arr[5][10]' and our reference is 'arr[0]'
            /// then the result type of 'arr[0]' is 'int[10]'.
            /// N is the amount of index expressions used when referencing the array
            /// here 'arr'.
            /// In the example it's 1.
            auto D = Type.GetDimensions();
            D.erase(D.begin(), D.begin() + IndexExpressions.size());
        }
        else
        {
            UndefinedSymbolError(Id, lexer);
        }

        return std::make_unique<ArrayExpression>(Id, IndexExpressions, Type);
    }
}

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
        auto RightExpression = ParsePrimaryExpression();

        /// In case of Assignment check if the left operand since if should be an
        /// lvalue. Which is either an identifier reference or an array expression.
        if (BinaryOperator.GetKind() == Token::Assign
            && !dynamic_cast<ReferenceExpression *>(LeftExpression.get())
            && !dynamic_cast<ArrayExpression *>(LeftExpression.get()))
        {
            EmitErrorWithLineInfoAndAffectedLine(
                "lvalue required as left operand of assignment", lexer);
        }

        int NextTokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        // ?
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
                    EmitErrorWithLineInfoAndAffectedLine("Type mismatch", lexer);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression), LeftType);
            }
            /// mod operation
            else if (BinaryOperator.GetKind() == Token::Mod)
            {
                // TODO
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

        LeftExpression = std::make_unique<BinaryExpression>(
            std::move(LeftExpression), BinaryOperator, std::move(RightExpression));
    }
}

// TODO: To report the location of error we would need the token holding the
// symbol name. It would be wise anyway to save it in AST nodes rather than
// just a string.
void Parser::InsertToSymbolTable(const std::string &SymbolName,
                                 ComplexType SymType,
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
        SymTabStack.InsertEntry(SymEntry);
    }
}
