using com.github.javaparser.ast;
using com.github.javaparser.ast.body;
using com.github.javaparser.ast.expr;
using com.github.javaparser.ast.type;
using JavaToCSharp.Statements;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace JavaToCSharp.Declarations;

public class ClassOrInterfaceDeclarationVisitor : BodyDeclarationVisitor<ClassOrInterfaceDeclaration>
{
    public override MemberDeclarationSyntax? VisitForClass(
        ConversionContext context,
        ClassDeclarationSyntax classSyntax,
        ClassOrInterfaceDeclaration declaration,
        IReadOnlyList<ClassOrInterfaceType> extends,
        IReadOnlyList<ClassOrInterfaceType> implements)
    {
        return VisitClassDeclaration(context, declaration);
    }

    public override MemberDeclarationSyntax? VisitForInterface(ConversionContext context,
        InterfaceDeclarationSyntax interfaceSyntax,
        ClassOrInterfaceDeclaration declaration)
    {
        return VisitClassDeclaration(context, declaration);
    }

    public static InterfaceDeclarationSyntax VisitInterfaceDeclaration(ConversionContext context,
        ClassOrInterfaceDeclaration interfaceDecl, bool isNested = false)
    {
        var originalTypeName = interfaceDecl.getName();
        var convertedName = TypeHelper.ConvertType(originalTypeName.getIdentifier());
        var newTypeName = context.Options.StartInterfaceNamesWithI
            ? $"I{convertedName}"
            : convertedName;

        if (context.Options.StartInterfaceNamesWithI)
        {
            TypeHelper.AddOrUpdateTypeNameConversions(originalTypeName.getIdentifier(), newTypeName);
        }

        if (!isNested)
        {
            context.RootTypeName = newTypeName;
        }

        context.LastTypeName = newTypeName;

        var classSyntax = SyntaxFactory.InterfaceDeclaration(newTypeName);

        var typeParams = interfaceDecl.getTypeParameters().ToList<TypeParameter>();

        if (typeParams is { Count: > 0 })
        {
            classSyntax = classSyntax.AddTypeParameterListParameters(typeParams
                    .Select(i => SyntaxFactory.TypeParameter(i.getNameAsString())).ToArray());
        }

        var mods = interfaceDecl.getModifiers().ToModifierKeywordSet();

        if (mods.Contains(Modifier.Keyword.PRIVATE))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));
        if (mods.Contains(Modifier.Keyword.PROTECTED))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword));
        if (mods.Contains(Modifier.Keyword.PUBLIC))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        if (mods.Contains(Modifier.Keyword.FINAL))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.SealedKeyword));

        var extends = interfaceDecl.getExtendedTypes().ToList<ClassOrInterfaceType>();

        if (extends is not null)
        {
            foreach (var extend in extends)
            {
                classSyntax = classSyntax.AddBaseListTypes(SyntaxFactory.SimpleBaseType(TypeHelper.GetSyntaxFromType(extend)));
            }
        }

        var implements = interfaceDecl.getImplementedTypes().ToList<ClassOrInterfaceType>();

        if (implements is not null)
        {
            foreach (var implement in implements)
            {
                classSyntax = classSyntax.AddBaseListTypes(SyntaxFactory.SimpleBaseType(TypeHelper.GetSyntaxFromType(implement)));
            }
        }

        var members = interfaceDecl.getMembers()?.ToList<BodyDeclaration>();

        if (members is not null)
        {
            foreach (var member in members)
            {
                var syntax = VisitBodyDeclarationForInterface(context, classSyntax, member);
                var memberWithComments = syntax?.WithJavaComments(context, member);

                if (memberWithComments is not null)
                {
                    classSyntax = classSyntax.AddMembers(memberWithComments);
                }
            }
        }

        return classSyntax.WithJavaComments(context, interfaceDecl);
    }

    public static ClassDeclarationSyntax VisitClassDeclaration(ConversionContext context,
        ClassOrInterfaceDeclaration classDecl, bool isNested = false)
    {
        string name = TypeHelper.ConvertType(classDecl.getNameAsString());

        if (!isNested)
        {
            context.RootTypeName = name;
        }

        context.LastTypeName = name;

        var classSyntax = SyntaxFactory.ClassDeclaration(name);

        var typeParams = classDecl.getTypeParameters().ToList<TypeParameter>();

        if (typeParams is { Count: > 0 })
        {
            classSyntax = classSyntax.AddTypeParameterListParameters(typeParams
                    .Select(i => SyntaxFactory.TypeParameter(i.getNameAsString())).ToArray());
            classSyntax = classSyntax.AddConstraintClauses(TypeHelper.GetTypeParameterListConstraints(typeParams).ToArray());
        }

        var mods = classDecl.getModifiers().ToModifierKeywordSet();

        if (mods.Contains(Modifier.Keyword.PRIVATE))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));
        if (mods.Contains(Modifier.Keyword.PROTECTED))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword));
        if (mods.Contains(Modifier.Keyword.PUBLIC))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        if (mods.Contains(Modifier.Keyword.ABSTRACT))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.AbstractKeyword));
        if (mods.Contains(Modifier.Keyword.FINAL))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.SealedKeyword));

        var extends = classDecl.getExtendedTypes().ToList<ClassOrInterfaceType>() ?? [];

        foreach (var extend in extends)
        {
            classSyntax = classSyntax.AddBaseListTypes(SyntaxFactory.SimpleBaseType(TypeHelper.GetSyntaxFromType(extend)));
        }

        var implements = classDecl.getImplementedTypes().ToList<ClassOrInterfaceType>() ?? [];

        foreach (var implement in implements)
        {
            classSyntax = classSyntax.AddBaseListTypes(SyntaxFactory.SimpleBaseType(TypeHelper.GetSyntaxFromType(implement)));
        }

        var members = classDecl.getMembers()?.ToList<BodyDeclaration>();

        // Collect static initializers to merge into a single static constructor
        var staticInitializers = members?.OfType<InitializerDeclaration>()
            .Where(i => i.isStatic()).ToList() ?? [];

        // Collect instance initializers to merge into a parameterless constructor
        var instanceInitializers = members?.OfType<InitializerDeclaration>()
            .Where(i => !i.isStatic()).ToList() ?? [];

        if (members is not null)
        {
            foreach (var member in members)
            {
                // Skip all initializers - they'll be merged below
                if (member is InitializerDeclaration)
                    continue;

                if (member is ClassOrInterfaceDeclaration childType)
                {
                    if (childType.isInterface())
                    {
                        var childInt = VisitInterfaceDeclaration(context, childType, true);

                        classSyntax = classSyntax.AddMembers(childInt);
                    }
                    else
                    {
                        var childClass = VisitClassDeclaration(context, childType, true);

                        classSyntax = classSyntax.AddMembers(childClass);
                    }
                }
                else
                {
                    var syntax = VisitBodyDeclarationForClass(context, classSyntax, member, extends, implements);
                    var withJavaComments = syntax?.WithJavaComments(context, member);

                    if (withJavaComments is not null)
                    {
                        classSyntax = classSyntax.AddMembers(withJavaComments);
                    }
                }

                while (context.PendingAnonymousTypes.Count > 0)
                {
                    var anon = context.PendingAnonymousTypes.Dequeue();
                    classSyntax = classSyntax.AddMembers(anon);
                }
            }
        }

        // Merge all static initializers into one static constructor
        if (staticInitializers.Count > 0)
        {
            var allStatements = staticInitializers
                .SelectMany(i => {
                    var block = (BlockSyntax)new BlockStatementVisitor().Visit(context, i.getBody());
                    return block.Statements;
                }).ToArray();

            var staticCtor = SyntaxFactory.ConstructorDeclaration(classSyntax.Identifier.ValueText)
                .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
                .WithBody(SyntaxFactory.Block(allStatements));

            classSyntax = classSyntax.AddMembers(staticCtor);
        }

        // Merge all instance initializers - prepend to existing constructors or create new one
        if (instanceInitializers.Count > 0)
        {
            var initStatements = instanceInitializers
                .SelectMany(i => {
                    var block = (BlockSyntax)new BlockStatementVisitor().Visit(context, i.getBody());
                    return block.Statements;
                }).ToArray();

            // Find existing constructors (non-static)
            var existingCtors = classSyntax.Members.OfType<ConstructorDeclarationSyntax>()
                .Where(c => !c.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword))).ToList();

            if (existingCtors.Count > 0)
            {
                // Prepend initializer statements to each existing constructor
                var newMembers = classSyntax.Members.Select(member =>
                {
                    if (member is ConstructorDeclarationSyntax ctor &&
                        !ctor.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword)))
                    {
                        var existingStatements = ctor.Body?.Statements ?? [];
                        var newBody = SyntaxFactory.Block(initStatements.Concat(existingStatements));
                        return ctor.WithBody(newBody);
                    }
                    return member;
                }).ToArray();

                classSyntax = classSyntax.WithMembers(SyntaxFactory.List(newMembers));
            }
            else
            {
                // No existing constructors - create a new parameterless one
                var instanceCtor = SyntaxFactory.ConstructorDeclaration(classSyntax.Identifier.ValueText)
                    .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithBody(SyntaxFactory.Block(initStatements));

                classSyntax = classSyntax.AddMembers(instanceCtor);
            }
        }

        var annotations = classDecl.getAnnotations().ToList<AnnotationExpr>();

        if (annotations is { Count: > 0 })
        {
            foreach (var annotation in annotations)
            {
                string annotationName = annotation.getNameAsString();
                const string annotationText = "Obsolete"; // TODO parse from java comment

                if (annotationName == "Deprecated")
                {
                    classSyntax = classSyntax.AddAttributeLists(SyntaxFactory.AttributeList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("Obsolete"))
                                    .WithArgumentList(
                                        SyntaxFactory.AttributeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList(
                                                SyntaxFactory.AttributeArgument(
                                                    SyntaxFactory.LiteralExpression(
                                                        SyntaxKind.StringLiteralExpression,
                                                        SyntaxFactory.Literal(annotationText)))))))));

                    break;
                }
            }
        }

        return classSyntax.WithJavaComments(context, classDecl);
    }
}
