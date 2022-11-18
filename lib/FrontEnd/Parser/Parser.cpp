#include "FrontEnd/Parser/Parser.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "Utils/ErrorLogger.hpp"
#include "fmt/core.h"
#include <algorithm>
#include <memory>

bool Parser::IsTypeSpecifier(Token T)
{
    switch (T.GetKind())
    {
        case Token::Char:
        case Token::Int:
        case Token::Long:
        case Token::Double:
        case Token::Unsigned:
        case Token::Struct: return true;

        case Token::Identifier: {
            auto Id = T.GetString();
            if (TypeDefinitions.count(Id) != 0)
                return true;
        }

        default: break;
    }

    return false;
}

static bool IsUnaryOperator(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::And:
        case Token::Minus:
        case Token::Not:
        case Token::Mul: return true;

        default: break;
    }

    return false;
}

bool Parser::IsReturnTypeSpecifier(Token T)
{
    return T.GetKind() == Token::Void || IsTypeSpecifier(T);
}

bool Parser::IsQualifer(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::TypeDef:
        case Token::Const: return true;

        default: break;
    }

    return false;
}

bool Parser::IsUserDefinedType(std::string Name)
{
    return UserDefinedTypes.count(Name) > 0 || TypeDefinitions.count(Name);
}

Type Parser::GetUserDefinedType(std::string Name)
{
    assert(IsUserDefinedType(Name));

    if (UserDefinedTypes.count(Name) > 0)
        return std::get<0>(UserDefinedTypes[Name]);
    else
        return TypeDefinitions[Name];
}

std::vector<std::string> Parser::GetUserDefinedTypeMembers(std::string Name)
{
    assert(IsUserDefinedType(Name));

    if (TypeDefinitions.count(Name) > 0)
        Name = TypeDefinitions[Name].GetName();

    return std::get<1>(UserDefinedTypes[Name]);
}

unsigned Parser::ParseQualifiers()
{
    unsigned Qualifiers   = 0;
    auto CurrentTokenKind = GetCurrentTokenKind();

    while (IsQualifer(CurrentTokenKind))
    {
        Lex();    // eat the qualifier token

        switch (CurrentTokenKind)
        {
            case Token::TypeDef: Qualifiers |= Type::TypeDef; break;
            case Token::Const: Qualifiers |= Type::Const; break;

            default: break;
        }

        CurrentTokenKind = GetCurrentTokenKind();
    }

    return Qualifiers;
}

Type Parser::ParseType(Token::TokenKind tk)
{
    Type Result;
    switch (tk)
    {
        case Token::Void: Result.SetTypeVariant(Type::Void); break;
        case Token::Char: Result.SetTypeVariant(Type::Char); break;
        case Token::Int: Result.SetTypeVariant(Type::Int); break;
        case Token::Double: Result.SetTypeVariant(Type::Double); break;
        case Token::Long: {
            auto NextTokenKind = lexer.LookAhead(2).GetKind();
            if (NextTokenKind == Token::Long)
            {
                Lex();    // eat 'long'
                Result.SetTypeVariant(Type::LongLong);
                break;
            }
            Result.SetTypeVariant(Type::Long);
            break;
        }
        case Token::Unsigned: {
            auto NextTokenKind = lexer.LookAhead(2).GetKind();
            if (NextTokenKind == Token::Int || NextTokenKind == Token::Char ||
                NextTokenKind == Token::Long)
            {
                Lex();    // eat 'unsigned'

                // if bare the 'unsigned' is not followed by other type then its an
                // 'unsigned int' by default
                // TODO: Investigate what else token could follow unsigned which would
                // certainly mean that the unsigned is alone and valid
            }
            else if (NextTokenKind == Token::Identifier)
            {
                Result.SetTypeVariant(Type::UnsignedInt);
                return Result;
            }
            else
            {
                assert(!"Error: Unexpected token after `unsigned`");
            }

            auto CurrentToken = lexer.GetCurrentToken();
            switch (CurrentToken.GetKind())
            {
                case Token::Char: Result.SetTypeVariant(Type::UnsignedChar); break;
                case Token::Int: Result.SetTypeVariant(Type::UnsignedInt); break;
                case Token::Long: {
                    auto NextTokenKind = lexer.LookAhead(2).GetKind();
                    if (NextTokenKind == Token::Long)
                    {
                        Lex();    // eat 'long';
                        Result.SetTypeVariant(Type::UnsignedLongLong);
                        break;
                    }
                    Result.SetTypeVariant(Type::UnsignedLong);
                    break;
                }

                default: assert(!"Unreachable");
            }
            break;
        }

        case Token::Struct: {
            Lex();    // eat 'struct'

            auto CurrentTK   = lexer.GetCurrentToken();
            std::string Name = CurrentTK.GetString();

            Result = std::get<0>(UserDefinedTypes[Name]);
            break;
        }

        case Token::Identifier: {
            // assuming we parsing the current token
            // TODO: Change this function expect the Token and not the TokenKind
            assert(GetCurrentTokenKind() == Token::Identifier);
            auto Id = GetCurrentToken().GetString();
            return TypeDefinitions[Id];
        }

        default: assert(!"Unknown token kind."); break;
    }

    return Result;
}

static void UndefinedSymbolError(Token sym, Lexer &L)
{
    static std::string Format = ">  {}:{}: error: Undefined Symbol `{}`.\n\t\t{}\n\n";

    PrintError(Format,
               sym.GetLine() + 1,
               sym.GetColumn() + 1,
               sym.GetString(),
               L.GetSource()[sym.GetLine()].substr(sym.GetColumn()));
}

[[maybe_unused]] static void ArrayTypeMismatchError(Token Sym, Type Actual)
{
    static std::string Format =
        ">  {}:{}: error: Function Type Mismatch `{}` , type is `{}`, it is not an array type.\n";

    PrintError(Format,
               Sym.GetLine() + 1,
               Sym.GetColumn() + 1,
               Sym.GetString(),
               Actual.ToString());
}

void static EmitError(const std::string &msg, Lexer &L)
{
    static std::string Format = ">  {}: error: `{}`.\n\t\t{}\n\n";

    PrintError(Format, L.GetLine(), msg, L.GetSource()[L.GetLine() - 1]);
}

void static EmitError(const std::string &msg, Lexer &L, Token &T)
{
    static std::string Format = ">  {}:{}: error: `{}`.\n\t\t{}\n\n";

    PrintError(Format,
               T.GetLine() + 1,
               T.GetColumn() + 1,
               msg,
               L.GetSource()[T.GetLine()]);
}

Token Parser::Expect(Token::TokenKind TKind)
{
    auto t = Lex();

    if (t.GetKind() != TKind)
    {
        std::string Format =
            ">  {}:{}: error: Unexpected Symbol `{}`. Expected is `{}`. \"{}\"\n\n";

        PrintError(Format,
                   t.GetLine() + 1,
                   t.GetColumn() + 1,
                   t.GetString(),
                   Token::ToString(TKind),
                   lexer.GetSource()[t.GetLine()]);
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
    auto TK                             = GetCurrentToken();

    while (IsReturnTypeSpecifier(TK) || lexer.Is(Token::Struct) ||
           lexer.Is(Token::Enum) || IsQualifer(TK.GetKind()))
    {
        auto Qualifiers = ParseQualifiers();
        TK              = GetCurrentToken();


        if (lexer.Is(Token::Struct) && lexer.LookAhead(3).GetKind() == Token::LeftBrace)
        {
            TU->AddDeclaration(ParseStructDeclaration(Qualifiers));
            TK = GetCurrentToken();
            continue;
        }

        if (lexer.Is(Token::Enum))
        {
            TU->AddDeclaration(ParseEnumDeclaration(Qualifiers));
            TK = GetCurrentToken();
            continue;
        }

        Type Ty            = ParseType(TK.GetKind());
        CurrentFuncRetType = Ty;
        Lex();

        auto Name    = Expect(Token::Identifier);
        auto NameStr = Name.GetString();

        if (Qualifiers & Type::TypeDef)
        {
            TypeDefinitions[NameStr] = Ty;
            Expect(Token::SemiColon);
            TK = GetCurrentToken();
            continue;
        }

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
            }

            if (!Dimensions.empty())
                Ty.SetDimensions(std::move(Dimensions));

            // If the variable initialized
            std::unique_ptr<Expression> InitExpr {nullptr};
            if (lexer.Is(Token::Assign))
            {
                Lex();

                if (lexer.Is(Token::LeftBrace))
                    InitExpr = ParseInitializerListExpression();
                else
                    InitExpr = ParseExpression();
            }

            Expect(Token::SemiColon);

            InsertToSymbolTable(NameStr, Ty);

            TU->AddDeclaration(
                std::make_unique<VariableDeclaration>(NameStr, Ty, std::move(InitExpr)));
        }

        TK = GetCurrentToken();
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
    InsertToSymbolTable(NameStr, FuncType, true);

    this->ReturnNumber = 0;
    std::unique_ptr<CompoundStatement> Body {nullptr};
    if (lexer.Is(Token::SemiColon))
        Lex();    // eat ';'
    else
        Body = ParseCompoundStatement();

    SymTabStack.PopSymbolTable();

    return std::make_unique<FunctionDeclaration>(FuncType,
                                                 NameStr,
                                                 PL,
                                                 Body,
                                                 ReturnNumber);
}

// <ParameterDeclaration> ::= { <TypeSpecifier> '*' <Identifier>? }?
std::unique_ptr<FunctionParameterDeclaration> Parser::ParseParameterDeclaration()
{
    std::unique_ptr<FunctionParameterDeclaration> FPD =
        std::make_unique<FunctionParameterDeclaration>();

    if (IsTypeSpecifier(GetCurrentToken()) || IsQualifer(GetCurrentTokenKind()))
    {
        unsigned Qualifiers = ParseQualifiers();
        Type Ty             = ParseTypeSpecifier();

        Ty.SetQualifiers(Qualifiers);
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

    if (!IsTypeSpecifier(GetCurrentToken()) && !IsQualifer(GetCurrentTokenKind()))
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
    auto TK = GetCurrentToken();

    unsigned Qualifiers = 0;
    if (IsQualifer(TK.GetKind()))
    {
        Qualifiers = ParseQualifiers();
        TK         = GetCurrentToken();
    }

    if (!IsTypeSpecifier(TK) || (Qualifiers & Type::TypeDef))
        assert(!"Invalid type");

    auto ParsedType = ParseType(TK.GetKind());
    ParsedType.SetQualifiers(Qualifiers);

    return ParsedType;
}

Node Parser::ParseReturnTypeSpecifier() { return Node(); }


// <VaraibleDeclaration> ::= <TypeSpecifier> <Identifier>
//                           {'[' <IntegerConstant> ]'}* { = <Expression> } ';'
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

    InsertToSymbolTable(Name, Ty);

    // If the variable initialized
    std::unique_ptr<Expression> InitExpr {nullptr};
    if (lexer.Is(Token::Assign))
    {
        Lex();    // eat `=`
        if (lexer.Is(Token::LeftBrace))
            InitExpr = ParseInitializerListExpression();
        else
        {
            InitExpr = ParseExpression();

            // if the variable type not match the size of the initializer expression
            // then also do an Implicit Cast.
            auto LHS = Ty.GetTypeVariant();
            auto RHS = InitExpr->GetResultType().GetTypeVariant();
            if ((LHS != RHS) && !Type::OnlySignednessDifference(LHS, RHS))
            {
                if (!Type::IsImplicitlyCastable(RHS, LHS))
                {
                    assert(!"Invalid initialization");
                }
                else
                {
                    InitExpr =
                        std::make_unique<ImplicitCastExpression>(std::move(InitExpr), Ty);
                }
            }
        }
    }

    Expect(Token::SemiColon);

    auto VD = std::make_unique<VariableDeclaration>(Name, Ty, Dimensions);

    if (InitExpr)
        VD->SetInitExpr(std::move(InitExpr));

    return VD;
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
std::unique_ptr<StructDeclaration> Parser::ParseStructDeclaration(unsigned Qualifiers)
{
    Expect(Token::Struct);

    std::string Name = Expect(Token::Identifier).GetString();

    Expect(Token::LeftBrace);    // eat '{'

    std::vector<std::unique_ptr<MemberDeclaration>> Members;
    Type type(Type::Struct);
    type.SetName(Name);
    type.SetQualifiers(Qualifiers);

    std::vector<std::string> StructMemberIdentifiers;
    while (lexer.IsNot(Token::RightBrace))
    {
        auto MD = ParseMemberDeclaration();
        type.GetTypeList().push_back(MD->GetType());
        StructMemberIdentifiers.push_back(MD->GetName());
        Members.push_back(std::move(MD));
    }

    Expect(Token::RightBrace);

    if (Qualifiers & Type::TypeDef)
    {
        auto AliasName             = Expect(Token::Identifier).GetString();
        TypeDefinitions[AliasName] = type;
    }

    Expect(Token::SemiColon);

    // saving the struct type and name
    UserDefinedTypes[Name] = {type, std::move(StructMemberIdentifiers)};

    return std::make_unique<StructDeclaration>(Name, Members, type);
}

// <EnumDeclaration> ::= 'enum' '{' <Identifier> (, <Identifier>)* '}' ';'
std::unique_ptr<EnumDeclaration> Parser::ParseEnumDeclaration(unsigned Qualifiers)
{
    Expect(Token::Enum);
    Expect(Token::LeftBrace);

    EnumDeclaration::EnumList Enumerators;

    int EnumCounter = 0;
    do
    {
        if (lexer.Is(Token::Comma))
            Lex();

        auto Identifier = Expect(Token::Identifier);
        Enumerators.push_back({Identifier.GetString(), EnumCounter});

        // Insert into the symbol table and for now assign the index of the enum
        // to it, not considering explicit assignments like "enum { A = 10 };"
        InsertToSymbolTable(Identifier.GetString(),
                            Type(Type::Int),
                            false,
                            ValueType((unsigned)EnumCounter));
        EnumCounter++;
    }
    while (lexer.Is(Token::Comma));

    Expect(Token::RightBrace);

    if (Qualifiers & Type::TypeDef)
    {
        auto AliasName             = Expect(Token::Identifier).GetString();
        TypeDefinitions[AliasName] = Type(Type::Int);
    }

    Expect(Token::SemiColon);

    return std::make_unique<EnumDeclaration>(Enumerators);
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
    if (lexer.Is(Token::Switch))
        return ParseSwitchStatement();
    if (lexer.Is(Token::Break))
        return ParseBreakStatement();
    if (lexer.Is(Token::Continue))
        return ParseContinueStatement();
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

// <SwitchStatement> ::= switch '(' <Expression> ')' '{'
//                       'case' <Constant> ':' <Statement>*
//                       'default' ':' <Statement>*
//                       '}'
std::unique_ptr<SwitchStatement> Parser::ParseSwitchStatement()
{
    std::unique_ptr<SwitchStatement> SS = std::make_unique<SwitchStatement>();

    Expect(Token::Switch);
    Expect(Token::LeftParen);

    SS->SetCondition(std::move(ParseExpression()));

    Expect(Token::RightParen);
    Expect(Token::LeftBrace);

    SwitchStatement::CasesDataVec CasesData;
    unsigned FoundDefaults = 0;

    while (lexer.Is(Token::Case) || lexer.Is(Token::Default))
    {
        const bool IsCase = lexer.Is(Token::Case);
        Lex();    // eat 'case' or 'default'

        std::unique_ptr<Expression> ConstExpr;

        if (IsCase)
        {
            ConstExpr = ParseConstantExpression();
            // For now if its not a constant then assuming its an enum const which is
            // an identifier currently handled by the below function
            // TODO: Move the handling of enum const to ParseConstantExpression maybe
            // but, this would also need some caution
            if (!ConstExpr)
                ConstExpr = ParseIdentifierExpression();
            // TODO: make it a semantic check and not assertion
            assert(ConstExpr.get()->GetResultType().IsIntegerType() &&
                   "Case expression must be an integer type");
        }

        Expect(Token::Colon);

        SwitchStatement::StmtsVec Statements;
        while (lexer.IsNot(Token::RightBrace) && lexer.IsNot(Token::Case) &&
               lexer.IsNot(Token::Default))
            Statements.push_back(std::move(ParseStatement()));

        if (IsCase)
        {
            int CaseConstVal =
                dynamic_cast<IntegerLiteralExpression *>(ConstExpr.get())->GetSIntValue();
            CasesData.push_back({CaseConstVal, std::move(Statements)});
        }
        else
        {
            FoundDefaults++;
            // TODO: Make it a semantic check
            assert(FoundDefaults <= 1 && "Too much default case!");
            SS->SetDefaultBody(std::move(Statements));
        }
    }

    SS->SetCasesBodies(std::move(CasesData));

    Expect(Token::RightBrace);

    return SS;
}

// <BreakStatement> ::= 'break' ';'
std::unique_ptr<BreakStatement> Parser::ParseBreakStatement()
{
    Expect(Token::Break);
    Expect(Token::SemiColon);

    return std::make_unique<BreakStatement>();
}

// <ContinueStatement> ::= 'continue' ';'
std::unique_ptr<ContinueStatement> Parser::ParseContinueStatement()
{
    Expect(Token::Continue);
    Expect(Token::SemiColon);
    return std::make_unique<ContinueStatement>();
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

// <ForStatement> ::= for '(' <Expression> ';' <Expression> ';' <Expression> ')'
//                            <Statement>
//                  | for '(' <VariableDeclaration> <Expression> ';'
//                            <Expression> ')' <Statement>
std::unique_ptr<ForStatement> Parser::ParseForStatement()
{
    std::unique_ptr<ForStatement> FS = std::make_unique<ForStatement>();

    Expect(Token::For);
    Expect(Token::LeftParen);

    SymTabStack.PushSymbolTable();

    // Parse variable declaration.
    if (IsTypeSpecifier(GetCurrentToken()))
        FS->SetVarDecl(std::move(ParseVariableDeclaration()));
    else
    {
        FS->SetInit(std::move(ParseExpression()));
        Expect(Token::SemiColon);
    }

    FS->SetCondition(std::move(ParseExpression()));
    Expect(Token::SemiColon);

    FS->SetIncrement(std::move(ParseExpression()));
    Expect(Token::RightParen);

    FS->SetBody(std::move(ParseStatement()));

    SymTabStack.PopSymbolTable();

    return FS;
}


// <ReturnStatement> ::= return <Expression>? ';'
// TODO: we need Explicit type conversioins here as well
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement()
{
    ReturnNumber++;
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

    std::vector<std::unique_ptr<Statement>> Statements;

    while (IsTypeSpecifier(GetCurrentToken()) || IsQualifer(GetCurrentTokenKind()) ||
           lexer.IsNot(Token::RightBrace))
    {
        if (IsTypeSpecifier(GetCurrentToken()) || IsQualifer(GetCurrentTokenKind()))
            Statements.push_back(std::move(ParseVariableDeclaration()));
        else
            Statements.push_back(std::move(ParseStatement()));
    }

    Expect(Token::RightBrace);

    return std::make_unique<CompoundStatement>(Statements);
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

static bool IsPostfixOperator(Token TK)
{
    switch (TK.GetKind())
    {
        case Token::Arrow:
        case Token::Inc:
        case Token::Dec:
        case Token::LeftParen:
        case Token::LeftBracket:
        case Token::Dot: return true;

        default: break;
    }

    return false;
}

// <PostFixExpression> ::= <PrimaryExpression>
//                       | <PostFixExpression> '++'
//                       | <PostFixExpression> '--'
//                       | <PostFixExpression> '(' <Arguments> ')'
//                       | <PostFixExpression> '[' <Expression> ']'
//                       | <PostFixExpression> '.' <Identifier>
//                       | <PostFixExpression> '->' <Identifier>
//                       | ( TypeName ) '{' <Initializer-List> '}'
std::unique_ptr<Expression> Parser::ParsePostFixExpression()
{
    auto CurrentToken = lexer.GetCurrentToken();

    // Struct Initializing case
    if (lexer.Is(Token::LeftParen) && lexer.LookAhead(2).GetKind() == Token::Identifier &&
        IsUserDefinedType(lexer.LookAhead(2).GetString()))
    {
        Expect(Token::LeftParen);
        auto UserDTypeName = Expect(Token::Identifier).GetString();
        Expect(Token::RightParen);

        Expect(Token::LeftBrace);

        std::vector<std::string> MemberList;
        StructInitExpression::ExprPtrList InitList;

        while (lexer.Is(Token::Dot) || lexer.Is(Token::Identifier))
        {
            std::string Member {};

            if (lexer.Is(Token::Dot))
            {
                Lex();    // eat '.'
                Member = Expect(Token::Identifier).GetString();
                Expect(Token::Assign);
            }

            MemberList.push_back(Member);
            InitList.push_back(ParseExpression());

            if (!lexer.Is(Token::Comma))
                break;

            Lex();    // eat ','
        }

        Expect(Token::RightBrace);

        // construct a list of the orders how the fields are initialized
        // example:
        //      ```
        //      struct P {int x; int y};
        //      ...
        //
        //      struct P Obj = (struct P) { .y = 2, .x = 1}
        //
        //      ```
        //  in the above case the InitOrder would look like: {1, 0}, so the first
        //  init expression actually initializing the 2nd (index 1) struct member

        auto MemberNames = GetUserDefinedTypeMembers(UserDTypeName);
        std::vector<unsigned> InitOrder;

        for (auto &Member : MemberList)
        {
            unsigned Order = 0;
            for (auto &TypeMemberName : MemberNames)
            {
                if (TypeMemberName == Member)
                {
                    InitOrder.push_back(Order);
                    break;
                }
                Order++;
            }
        }

        return std::make_unique<StructInitExpression>(GetUserDefinedType(UserDTypeName),
                                                      std::move(InitList),
                                                      std::move(InitOrder));
    }

    auto Expr = ParsePrimaryExpression();
    assert(Expr && "Cannot be NULL");

    while (IsPostfixOperator(lexer.GetCurrentToken()))
    {
        if (lexer.Is(Token::Inc) || lexer.Is(Token::Dec))
        {
            auto Operation = lexer.GetCurrentToken();

            Lex();
            Expr->SetLValueness(true);
            Expr = std::make_unique<UnaryExpression>(Operation, std::move(Expr));
        }
        // Parse a CallExpression here
        else if (lexer.Is(Token::LeftParen))
        {
            Expr = ParseCallExpression(CurrentToken);
        }
        // parse ArrayExpression
        else if (lexer.Is(Token::LeftBracket))
        {
            Expr = ParseArrayExpression(std::move(Expr));
        }
        // parse StructMemberAccess
        else if (lexer.Is(Token::Dot) || lexer.Is(Token::Arrow))
        {
            const bool IsArrow = lexer.Is(Token::Arrow);

            Lex();    // eat the token
            auto MemberId    = Expect(Token::Identifier);
            auto MemberIdStr = MemberId.GetString();

            assert(Expr->GetResultType().IsStruct() && "TODO: emit error");
            assert((!IsArrow || (IsArrow && Expr->GetResultType().IsPointerType())) &&
                   "struct pointer expected");

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

            if (Expr->GetResultType().IsStruct() || Expr->GetResultType().IsArray())
                Expr->SetLValueness(true);
        }
    }

    return Expr;
}

static int GetBinOpPrecedence(Token::TokenKind TK)
{
    switch (TK)
    {
        case Token::Assign:
        case Token::PlusEqual:
        case Token::MinusEuqal:
        case Token::MulEqual:
        case Token::DivEqual: return 10;
        case Token::LogicalAnd: return 20;
        case Token::And: return 30;

        // ==, != , <=, >=
        case Token::GreaterEqual:
        case Token::LessEqual:
        case Token::Equal:
        case Token::NotEqual: return 40;

        /// <, >
        case Token::Less:
        case Token::Greater: return 50;

        case Token::LeftShift:
        case Token::RightShift: return 60;

        case Token::Plus:
        case Token::Minus: return 70;

        case Token::Mul:
        case Token::Div:
        case Token::Mod: return 80;

        default: return -1;
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

    // TODO: learn llvm
    if (lexer.Is(Token::Cond))
        LeftExpression = ParseTernaryExpression(std::move(LeftExpression));

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
        // If the symbol value is a know constant like in case of enumerators, then
        // return just a constant expression
        // TODO: Maybe do ths check earlier to save ourself from creating RE for
        // nothing
        if (auto Val = std::get<2>(SymEntry.value()); !Val.IsEmpty())
            return std::make_unique<IntegerLiteralExpression>(Val.GetIntVal());

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

// <InitializerListExpression> ::= '{' {<ConstantExpression> |
//                                      <InitializerListExpression>}
//                                     {',' {<ConstantExpression> |
//                                      <InitializerListExpression>} }* '}'
std::unique_ptr<Expression> Parser::ParseInitializerListExpression()
{
    Expect(Token::LeftBrace);

    std::unique_ptr<Expression> E;
    if (lexer.Is(Token::LeftBrace))
        E = ParseInitializerListExpression();
    else
        E = ParseConstantExpression();

    assert(E && "Cannot be null");

    std::vector<ExprPtr> ExprList;
    ExprList.push_back(std::move(E));

    while (lexer.Is(Token::Comma))
    {
        Lex();    // eat ','
        if (lexer.Is(Token::LeftBrace))
            ExprList.push_back(std::move(ParseInitializerListExpression()));
        else
            ExprList.push_back(std::move(ParseConstantExpression()));
    }

    Expect(Token::RightBrace);

    return std::make_unique<InitializerListExpression>(std::move(ExprList));
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
            auto CallArgType = CallArgs[i]->GetResultType();

            // If the ith argument type is not matching the expected one
            if (CallArgType != FuncArgTypes[i])
            {
                // Cast if allowed
                if (Type::IsImplicitlyCastable(CallArgType, FuncArgTypes[i]))
                    CallArgs[i] =
                        std::make_unique<ImplicitCastExpression>(std::move(CallArgs[i]),
                                                                 FuncArgTypes[i]);
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

    Type type = Base->GetResultType();

    /// Remove the first N dimensions from the actual type. Example:
    /// ActualType is 'int arr[5][10]' and our reference is 'arr[0]'
    /// then the result type of 'arr[0]' is 'int[10]'. N is the
    /// amount of index expressions used when referencing the array here
    /// 'arr'. In the example its 1.
    if (!type.IsPointerType())
        type.GetDimensions().erase(type.GetDimensions().begin(),
                                   type.GetDimensions().begin() + 1);

    else
        type.DecrementPointerLevel();

    Base->SetLValueness(true);
    return std::make_unique<ArrayExpression>(Base, IndexExpr, type);
}

// <ConstantExpression> ::= '-'? [1-9][0-9]*
//                        | '-'? [0-9]+.[0-9]+
std::unique_ptr<Expression> Parser::ParseConstantExpression()
{
    bool IsNegative = false;
    if (lexer.Is(Token::Minus))
    {
        Lex();
        IsNegative = true;
    }

    if (lexer.Is(Token::Identifier))
    {
        auto Id    = Expect(Token::Identifier);
        auto IdStr = Id.GetString();

        if (auto SymEntry = SymTabStack.Contains(IdStr))
        {
            if (auto Val = std::get<2>(SymEntry.value()); !Val.IsEmpty())
            {
                auto EnumILExpr =
                    std::make_unique<IntegerLiteralExpression>(Val.GetIntVal());
                if (IsNegative)
                    EnumILExpr->SetValue(-EnumILExpr->GetSIntValue());

                return EnumILExpr;
            }
            else
                assert(!"Not an enumerator constant");
        }
        else
            assert(!"Not an enumerator constant");
    }
    else if (lexer.Is(Token::Integer))
    {
        auto IntLitExpr =
            std::make_unique<IntegerLiteralExpression>(ParseIntegerConstant());

        if (IsNegative)
            IntLitExpr->SetValue(-IntLitExpr->GetSIntValue());

        // TODO: example:
        // currently `1 ull` would be valid since the lexer will ignore the
        // white spaces, make such input invalid
        if (lexer.Is(Token::Identifier))
        {
            auto Str = GetCurrentToken().GetString();
            if (Str == "u")
            {
                Lex();
                IntLitExpr->SetResultType(Type::UnsignedInt);
            }
            else if (Str == "l")
            {
                Lex();
                IntLitExpr->SetResultType(Type::Long);
            }
            else if (Str == "ul")
            {
                Lex();
                IntLitExpr->SetResultType(Type::UnsignedLong);
            }
            else if (Str == "ll")
            {
                Lex();
                IntLitExpr->SetResultType(Type::LongLong);
            }
            else if (Str == "ull")
            {
                Lex();
                IntLitExpr->SetResultType(Type::UnsignedLongLong);
            }
        }

        return IntLitExpr;
    }
    else
    {
        auto FPLitExpr = std::make_unique<FloatLiteralExpression>(ParseRealConstant());

        if (IsNegative)
            FPLitExpr->SetValue(-FPLitExpr->GetValue());

        return FPLitExpr;
    }
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

        bool IsArithmetic = BinaryOperator.IsArithmetic(BinaryOperator.GetKind());

        if (IsArithmetic &&
            Type::IsSmallerThanInt(LeftExpression->GetResultType().GetTypeVariant()))
        {
            LeftExpression =
                std::make_unique<ImplicitCastExpression>(std::move(LeftExpression),
                                                         Type(Type::Int));
        }

        if (IsArithmetic &&
            Type::IsSmallerThanInt(RightExpression->GetResultType().GetTypeVariant()))
        {
            RightExpression =
                std::make_unique<ImplicitCastExpression>(std::move(RightExpression),
                                                         Type(Type::Int));
        }

        /// In case of Assignment check if the left operand since if should be an
        /// lvalue. Which is either an identifier reference or an array expression.
        if (BinaryOperator.GetKind() == Token::Assign &&
            !dynamic_cast<ReferenceExpression *>(LeftExpression.get()) &&
            !dynamic_cast<ArrayExpression *>(LeftExpression.get()) &&
            !dynamic_cast<StructMemberReference *>(LeftExpression.get()))
        {
            EmitError("lvalue required as left operand of assignment",
                      lexer,
                      BinaryOperator);
        }

        /// If it's an Assign BinaryOperator and the left hand side is an
        /// ArrayExpression or ReferenceExpression, then it's an LValue. This can
        /// reduces one time load instruction generate for global Variable.

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

        if (LeftType != RightType && !Type::OnlySignednessDifference(LeftType, RightType))
        {
            /// if an assignment, then try to cast the RHS to type of LHS.
            if (BinaryOperator.GetKind() == Token::Assign)
            {
                if (!Type::IsImplicitlyCastable(RightType, LeftType))
                    EmitError("FuncType mismatch", lexer, BinaryOperator);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression),
                        LeftType);
            }
            /// Otherwise cast the one with lower conversion rank to higher one .
            else
            {
                auto DesiredType =
                    Type::GetStrongestType(LeftType, RightType).GetTypeVariant();

                // if LHS needs the conversion.
                if (LeftType != DesiredType)
                    LeftExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(LeftExpression),
                        DesiredType);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression),
                        DesiredType);
            }
        }
        /// mod operation
        else if (BinaryOperator.GetKind() == Token::Mod)
        {
            if (LeftType != Type::Int || RightType != Type::Int)
                ;    // TODO: fix this semantic check
                     // EmitError("Mod Operator can only operator on integers",
                     //           lexer,
                     //           BinaryOperator);
        }

        // TODO: This will only work here if the ternary condition was in
        //  parenthesis, which for the time being is sufficient. Make it work as it
        //  should.
        if (lexer.Is(Token::Cond))
            RightExpression = ParseTernaryExpression(std::move(RightExpression));

        LeftExpression = std::make_unique<BinaryExpression>(std::move(LeftExpression),
                                                            BinaryOperator,
                                                            std::move(RightExpression));
    }
}

// <TernaryExpression> ::= <Expression> '?' <Expression> ':' <Expression>
std::unique_ptr<Expression> Parser::ParseTernaryExpression(ExprPtr Condition)
{
    Expect(Token::Cond);

    auto TrueExpr = ParseExpression();
    Expect(Token::Colon);
    auto FalseExpr = ParseExpression();

    return std::make_unique<TernaryExpression>(Condition, TrueExpr, FalseExpr);
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
        PrintError("Error: Symbol `{}` with type `{}` is already defined.\n\n",
                   SymbolName,
                   SymType.ToString());
    }
    else
    {
        ToGlobal ? SymTabStack.InsertGlobalEntry(SymEntry) :
                   SymTabStack.InsertEntry(SymEntry);
    }
}
