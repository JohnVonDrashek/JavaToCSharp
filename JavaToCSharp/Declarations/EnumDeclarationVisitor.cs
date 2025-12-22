using com.github.javaparser;
using com.github.javaparser.ast;
using com.github.javaparser.ast.body;
using com.github.javaparser.ast.type;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace JavaToCSharp.Declarations;

public class EnumDeclarationVisitor : BodyDeclarationVisitor<EnumDeclaration>
{
    public override MemberDeclarationSyntax VisitForClass(
        ConversionContext context,
        ClassDeclarationSyntax? classSyntax,
        EnumDeclaration enumDecl,
        IReadOnlyList<ClassOrInterfaceType> extends,
        IReadOnlyList<ClassOrInterfaceType> implements)
    {
        return VisitEnumDeclaration(context, enumDecl);
    }

    public override MemberDeclarationSyntax VisitForInterface(ConversionContext context,
        InterfaceDeclarationSyntax interfaceSyntax, EnumDeclaration declaration)
    {
        return VisitEnumDeclaration(context, declaration);
    }

    public static MemberDeclarationSyntax VisitEnumDeclaration(ConversionContext context, EnumDeclaration enumDecl)
    {
        var name = enumDecl.getNameAsString();
        context.LastTypeName = name;

        var enumSyntax = SyntaxFactory.EnumDeclaration(name);

        var typeConstants = enumDecl.getEntries().ToList<EnumConstantDeclaration>();

        // If any entry has constructor args, convert to sealed class pattern
        if (typeConstants?.Any(e => !e.getArguments().isEmpty()) == true)
        {
            return VisitRichEnumAsClass(context, enumDecl, name, typeConstants);
        }

        if (typeConstants is { Count: > 0 })
        {
            var useCodeToComment = context.Options.UseUnrecognizedCodeToComment;
            var membersCount = typeConstants.Count;
            var enumMembers = new List<EnumMemberDeclarationSyntax>(membersCount);
            var lastMembersIndex = membersCount - 1;
            var showNoPortedWarning = false;

            for (int i = 0; i < membersCount; i++)
            {
                var itemConst = typeConstants[i];
                var memberDecl = SyntaxFactory.EnumMemberDeclaration(itemConst.getNameAsString())
                    .WithJavaComments(context, itemConst);

                if (useCodeToComment)
                {
                    //java-enum `body/args` to `code Comment`
                    var constArgs = itemConst.getArguments();
                    var classBody = itemConst.getClassBody();
                    if (!constArgs.isEmpty() || !classBody.isEmpty())
                    {
                        var bodyCodes = CommentsHelper.ConvertToComment([itemConst], "enum member body", false);

                        if (memberDecl.HasLeadingTrivia)
                        {
                            var firstLeadingTrivia = memberDecl.GetLeadingTrivia().Last();
                            memberDecl = memberDecl.InsertTriviaAfter(firstLeadingTrivia, bodyCodes);
                        }
                        else
                        {
                            memberDecl = memberDecl.WithLeadingTrivia(bodyCodes);
                        }

                        showNoPortedWarning = true;
                    }

                    //java-enum `method-body` to `code Comment`
                    if (i == lastMembersIndex)
                    {
                        memberDecl = MembersToCommentTrivia(memberDecl, ref showNoPortedWarning);
                    }
                }

                enumMembers.Add(memberDecl);
            }

            if (showNoPortedWarning)
                context.Options.Warning($"Members found in enum {name} will not be ported. Check for correctness.",
                    enumDecl.getBegin().FromRequiredOptional<Position>().line);

            enumSyntax = enumSyntax.AddMembers(enumMembers.ToArray());
        }

        var mods = enumDecl.getModifiers().ToModifierKeywordSet();

        if (mods.Contains(Modifier.Keyword.PRIVATE))
            enumSyntax = enumSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));
        if (mods.Contains(Modifier.Keyword.PROTECTED))
            enumSyntax = enumSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword));
        if (mods.Contains(Modifier.Keyword.PUBLIC))
            enumSyntax = enumSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));

        return enumSyntax.WithJavaComments(context, enumDecl);

        EnumMemberDeclarationSyntax MembersToCommentTrivia(EnumMemberDeclarationSyntax lastMemberDecl,
            ref bool showNoPortedWarning)
        {
            var members = enumDecl.getMembers().ToList<BodyDeclaration>();
            if (members is { Count: > 0 })
            {
                var todoCodes = CommentsHelper.ConvertToComment(members, "enum body members");
                var lastMemberTrailingTrivia = lastMemberDecl.GetTrailingTrivia();
                lastMemberDecl = lastMemberTrailingTrivia.Count > 0
                    ? lastMemberDecl.InsertTriviaAfter(lastMemberTrailingTrivia.Last(), todoCodes)
                    : lastMemberDecl.WithTrailingTrivia(todoCodes);
                showNoPortedWarning = true;
            }

            return lastMemberDecl;
        }
    }

    private static ClassDeclarationSyntax VisitRichEnumAsClass(
        ConversionContext context,
        EnumDeclaration enumDecl,
        string name,
        List<EnumConstantDeclaration> entries)
    {
        // Create sealed class with same modifiers
        var classSyntax = SyntaxFactory.ClassDeclaration(name)
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.SealedKeyword));

        var mods = enumDecl.getModifiers().ToModifierKeywordSet();
        if (mods.Contains(Modifier.Keyword.PUBLIC))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        if (mods.Contains(Modifier.Keyword.PROTECTED))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword));
        if (mods.Contains(Modifier.Keyword.PRIVATE))
            classSyntax = classSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));

        var membersList = new List<MemberDeclarationSyntax>();

        // Generate static readonly field for each enum constant
        foreach (var entry in entries)
        {
            var entryName = entry.getNameAsString();
            var args = entry.getArguments();

            var argList = args.isEmpty()
                ? SyntaxFactory.ArgumentList()
                : TypeHelper.GetSyntaxFromArguments(context, args);

            var initExpr = SyntaxFactory.ObjectCreationExpression(SyntaxFactory.IdentifierName(name))
                .WithArgumentList(argList);

            var fieldSyntax = SyntaxFactory.FieldDeclaration(
                SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(name))
                    .AddVariables(SyntaxFactory.VariableDeclarator(entryName)
                        .WithInitializer(SyntaxFactory.EqualsValueClause(initExpr))))
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));

            membersList.Add(fieldSyntax);
        }

        // Convert enum fields and constructors
        var members = enumDecl.getMembers().ToList<BodyDeclaration>() ?? [];
        var emptyList = Array.Empty<ClassOrInterfaceType>();
        var hasConstructor = false;

        foreach (var member in members)
        {
            if (member is FieldDeclaration)
            {
                var fieldSyntax = BodyDeclarationVisitor.VisitBodyDeclarationForClass(
                    context, classSyntax, member, emptyList, emptyList);
                if (fieldSyntax != null)
                    membersList.Add(fieldSyntax);
            }
            else if (member is ConstructorDeclaration)
            {
                var ctorSyntax = BodyDeclarationVisitor.VisitBodyDeclarationForClass(
                    context, classSyntax, member, emptyList, emptyList) as ConstructorDeclarationSyntax;
                if (ctorSyntax != null)
                {
                    // Force private modifier for enum constructors
                    ctorSyntax = ctorSyntax
                        .WithModifiers(SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.PrivateKeyword)));
                    membersList.Add(ctorSyntax);
                    hasConstructor = true;
                }
            }
            else if (member is MethodDeclaration)
            {
                var methodSyntax = BodyDeclarationVisitor.VisitBodyDeclarationForClass(
                    context, classSyntax, member, emptyList, emptyList);
                if (methodSyntax != null)
                    membersList.Add(methodSyntax);
            }
        }

        // Fallback: if no constructor found, add empty private constructor
        if (!hasConstructor)
        {
            var ctorSyntax = SyntaxFactory.ConstructorDeclaration(name)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword))
                .WithBody(SyntaxFactory.Block());
            membersList.Add(ctorSyntax);
        }

        classSyntax = classSyntax.AddMembers(membersList.ToArray());

        return classSyntax.WithJavaComments(context, enumDecl);
    }
}
