using com.github.javaparser.ast;
using com.github.javaparser.ast.body;
using com.github.javaparser.ast.expr;
using com.github.javaparser.ast.stmt;
using com.github.javaparser.ast.type;
using JavaToCSharp.Statements;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Parameter = com.github.javaparser.ast.body.Parameter;
using TypeParameter = com.github.javaparser.ast.type.TypeParameter;

namespace JavaToCSharp.Declarations;

public class MethodDeclarationVisitor : BodyDeclarationVisitor<MethodDeclaration>
{
    public override MemberDeclarationSyntax VisitForClass(
        ConversionContext context,
        ClassDeclarationSyntax classSyntax,
        MethodDeclaration methodDecl,
        IReadOnlyList<ClassOrInterfaceType> extends,
        IReadOnlyList<ClassOrInterfaceType> implements)
    {
        return VisitInternal(context, false, classSyntax.Identifier.Text, classSyntax.Modifiers, methodDecl, extends, implements);
    }

    public override MemberDeclarationSyntax VisitForInterface(ConversionContext context,
        InterfaceDeclarationSyntax interfaceSyntax,
        MethodDeclaration methodDecl)
    {
        // If there is a body, mostly treat it like a class method
        if (methodDecl.getBody().isPresent())
        {
            return VisitInternal(context, true, interfaceSyntax.Identifier.Text, interfaceSyntax.Modifiers, methodDecl,
                ArraySegment<ClassOrInterfaceType>.Empty, ArraySegment<ClassOrInterfaceType>.Empty);
        }

        var returnType = methodDecl.getType();
        var returnTypeName = TypeHelper.ConvertType(returnType.toString());

        var methodName = TypeHelper.Capitalize(methodDecl.getNameAsString());
        methodName = TypeHelper.ReplaceCommonMethodNames(methodName);

        var methodSyntax = SyntaxFactory.MethodDeclaration(SyntaxFactory.ParseTypeName(returnTypeName), methodName);

        // Add type parameters and constraints
        var typeParams = methodDecl.getTypeParameters().ToList<TypeParameter>();
        if (typeParams is { Count: > 0 })
        {
            methodSyntax = methodSyntax.AddTypeParameterListParameters(typeParams
                .Select(i => SyntaxFactory.TypeParameter(i.getNameAsString())).ToArray());
            methodSyntax = methodSyntax.AddConstraintClauses(
                TypeHelper.GetTypeParameterListConstraints(typeParams).ToArray());
        }

        var parameters = methodDecl.getParameters().ToList<Parameter>();

        if (parameters is {Count: > 0})
        {
            var paramSyntax = parameters.Select(i =>
                SyntaxFactory.Parameter(
                    attributeLists: [],
                    modifiers: SyntaxFactory.TokenList(),
                    type: SyntaxFactory.ParseTypeName(TypeHelper.ConvertTypeOf(i)),
                    identifier: SyntaxFactory.ParseToken(TypeHelper.EscapeIdentifier(i.getNameAsString())),
                    @default: null))
                .ToArray();

            methodSyntax = methodSyntax.AddParameterListParameters(paramSyntax.ToArray());
        }

        methodSyntax = methodSyntax.WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));

        return methodSyntax;
    }

    private static MemberDeclarationSyntax VisitInternal(
        ConversionContext context,
        bool isInterface,
        string typeIdentifier,
        SyntaxTokenList typeModifiers,
        MethodDeclaration methodDecl,
        IReadOnlyList<ClassOrInterfaceType> extends,
        IReadOnlyList<ClassOrInterfaceType> implements)
    {
        var returnType = methodDecl.getType();
        var returnTypeName = TypeHelper.ConvertType(returnType.toString());

        var methodName = TypeHelper.Capitalize(methodDecl.getNameAsString());
        methodName = TypeHelper.ReplaceCommonMethodNames(methodName);

        var methodSyntax = SyntaxFactory.MethodDeclaration(SyntaxFactory.ParseTypeName(returnTypeName), methodName);

        // Add type parameters and constraints
        var typeParams = methodDecl.getTypeParameters().ToList<TypeParameter>();
        if (typeParams is { Count: > 0 })
        {
            methodSyntax = methodSyntax.AddTypeParameterListParameters(typeParams
                .Select(i => SyntaxFactory.TypeParameter(i.getNameAsString())).ToArray());
            methodSyntax = methodSyntax.AddConstraintClauses(
                TypeHelper.GetTypeParameterListConstraints(typeParams).ToArray());
        }

        var mods = methodDecl.getModifiers().ToModifierKeywordSet();

        if (mods.Contains(Modifier.Keyword.PUBLIC))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        if (mods.Contains(Modifier.Keyword.PROTECTED))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword));
        if (mods.Contains(Modifier.Keyword.PRIVATE))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));
        if (mods.Contains(Modifier.Keyword.STATIC))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.StaticKeyword));
        if (mods.Contains(Modifier.Keyword.ABSTRACT))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.AbstractKeyword));

        var annotations = methodDecl.getAnnotations().ToList<AnnotationExpr>();
        bool isOverride = false;

        if (annotations is { Count: > 0 })
        {
            foreach (var annotation in annotations)
            {
                string name = annotation.getNameAsString();

                if (name == "Override")
                {
                    // Only add override if extending a concrete class (not just implementing interfaces)
                    // Java uses @Override for both, but C# only uses override for base class methods
                    bool hasConcreteBase = extends.Count > 0 &&
                        extends.Any(e => e.getNameAsString() != "Object");

                    if (hasConcreteBase)
                    {
                        var javaMethodName = methodDecl.getNameAsString().ToLower();

                        // If the class implements interfaces, we can't easily tell if the method
                        // is from the base class or an interface. Use conservative approach:
                        // only add override for methods that are definitely from Object/base classes
                        bool isKnownBaseClassMethod = javaMethodName is
                            "tostring" or "hashcode" or "equals" or "clone" or "compareto" or
                            "finalize" or "getclass" or "notify" or "notifyall" or "wait";

                        if (implements.Count == 0)
                        {
                            // No interfaces - safe to add override for all @Override methods
                            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.OverrideKeyword));
                            isOverride = true;
                        }
                        else if (isKnownBaseClassMethod)
                        {
                            // Has interfaces, but this is definitely a base class method
                            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.OverrideKeyword));
                            isOverride = true;
                        }
                        // else: has interfaces and unknown method - skip override to be safe
                    }
                }
                // add annotation if a mapping is found (empty mapping means suppress the annotation)
                else if (context.Options is not null && context.Options.SyntaxMappings.AnnotationMappings.TryGetValue(name, out var mappedAnnotation)
                         && !string.IsNullOrEmpty(mappedAnnotation))
                {
                    var attributeList = SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Attribute(SyntaxFactory.ParseName(mappedAnnotation))));
                    methodSyntax = methodSyntax.AddAttributeLists(attributeList);
                }
            }
        }

        if (!mods.Contains(Modifier.Keyword.FINAL)
            && !mods.Contains(Modifier.Keyword.ABSTRACT)
            && !mods.Contains(Modifier.Keyword.STATIC)
            && !mods.Contains(Modifier.Keyword.PRIVATE)
            && !isOverride
            && !isInterface
            && !typeModifiers.Any(i => i.IsKind(SyntaxKind.SealedKeyword)))
            methodSyntax = methodSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.VirtualKeyword));

        var parameters = methodDecl.getParameters().ToList<Parameter>();

        if (parameters is {Count: > 0})
        {
            var paramSyntaxes = new List<ParameterSyntax>();

            foreach (var param in parameters)
            {
                var type = param.getType();
                int arrayLevel = type.getArrayLevel();
                string identifier = TypeHelper.EscapeIdentifier(param.getNameAsString());

                if (param.isVarArgs() && arrayLevel == 0)
                {
                    arrayLevel = 1;
                }

                var modifiers = SyntaxFactory.TokenList();

                if (param.isVarArgs())
                    modifiers = SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.ParamsKeyword));

                var paramSyntax = SyntaxFactory.Parameter(
                    attributeLists: [],
                    modifiers: modifiers,
                    type: TypeHelper.ConvertTypeSyntax(type, arrayLevel),
                    identifier: SyntaxFactory.ParseToken(identifier),
                    @default: null);

                paramSyntaxes.Add(paramSyntax);
            }

            methodSyntax = methodSyntax.AddParameterListParameters(paramSyntaxes.ToArray());
        }

        var block = methodDecl.getBody().FromOptional<BlockStmt>();

        if (block is null)
        {
            // i.e. abstract method
            methodSyntax = methodSyntax.WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));

            return methodSyntax;
        }

        var statements = block.getStatements().ToList<Statement>();

        var statementSyntax = StatementVisitor.VisitStatements(context, statements);

        if (mods.Contains(Modifier.Keyword.SYNCHRONIZED))
        {
            var lockBlock = SyntaxFactory.Block(statementSyntax);

            var lockSyntax = mods.Contains(Modifier.Keyword.STATIC)
                ? SyntaxFactory.LockStatement(SyntaxFactory.TypeOfExpression(SyntaxFactory.ParseTypeName(typeIdentifier)), lockBlock)
                : SyntaxFactory.LockStatement(SyntaxFactory.ThisExpression(), lockBlock);

            methodSyntax = methodSyntax.AddBodyStatements(lockSyntax);
        }
        else
        {
            methodSyntax = methodSyntax.AddBodyStatements(statementSyntax.ToArray());
        }

        return methodSyntax;
    }
}
