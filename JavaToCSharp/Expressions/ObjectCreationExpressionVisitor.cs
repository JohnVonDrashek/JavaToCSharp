using com.github.javaparser.ast.body;
using com.github.javaparser.ast.expr;
using JavaToCSharp.Declarations;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using com.github.javaparser.ast;
using com.github.javaparser.ast.type;

namespace JavaToCSharp.Expressions;

public class ObjectCreationExpressionVisitor : ExpressionVisitor<ObjectCreationExpr>
{
    protected override ExpressionSyntax Visit(ConversionContext context, ObjectCreationExpr newExpr)
    {
        var anonBody = newExpr.getAnonymousClassBody().FromOptional<NodeList>().ToList<BodyDeclaration>();

        if (anonBody is { Count: > 0 })
        {
            return VisitAnonymousClassCreationExpression(context, newExpr, anonBody);
        }

        // TODO: what to do with scope?
        //var scope = newExpr.getScope();
        //ExpressionSyntax scopeSyntax = null;

        //if (scope is not null) {
        //    scopeSyntax = ExpressionVisitor.VisitExpression(context, scope);
        //}

        var type = newExpr.getType();
        var args = newExpr.getArguments();
        var typeArgs = type.getTypeArguments().FromOptional<NodeList>()?.ToList<com.github.javaparser.ast.type.Type>();

        // Use target-typed new (C# 9+) when type args are missing/empty (Java diamond operator)
        // This lets C# infer the type from context, e.g., List<Item> items = new();
        if (typeArgs is null || typeArgs.Count == 0)
        {
            var argList = (args is null || args.size() == 0)
                ? SyntaxFactory.ArgumentList()
                : TypeHelper.GetSyntaxFromArguments(context, args);
            return SyntaxFactory.ImplicitObjectCreationExpression()
                .WithArgumentList(argList);
        }

        var typeSyntax = TypeHelper.GetSyntaxFromType(type);

        if (args is null || args.size() == 0)
        {
            return SyntaxFactory.ObjectCreationExpression(typeSyntax).WithArgumentList(SyntaxFactory.ArgumentList());
        }

        return SyntaxFactory.ObjectCreationExpression(typeSyntax, TypeHelper.GetSyntaxFromArguments(context, args), null);
    }

    private static ObjectCreationExpressionSyntax VisitAnonymousClassCreationExpression(ConversionContext context, ObjectCreationExpr newExpr, List<BodyDeclaration> anonBody)
    {
        // Get the simple name for the anonymous type name (e.g., "Comparator" -> "AnonymousComparator")
        string simpleTypeName = TypeHelper.ConvertType(newExpr.getType().getNameAsString());

        // Get the full type syntax including generic arguments for the base type
        var baseTypeSyntax = TypeHelper.GetSyntaxFromType(newExpr.getType());

        string anonTypeName = string.Empty;

        for (int i = 0; i <= 100; i++)
        {
            if (i == 100)
            {
                throw new InvalidOperationException("Too many anonymous types");
            }

            anonTypeName = $"Anonymous{simpleTypeName}{(i == 0 ? string.Empty : i.ToString())}";

            if (context.UsedAnonymousTypeNames.Add(anonTypeName))
            {
                break; // go with this one
            }
        }

        var classSyntax = SyntaxFactory.ClassDeclaration(anonTypeName)
            .AddModifiers(
                SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                SyntaxFactory.Token(SyntaxKind.SealedKeyword))
            .WithBaseList(SyntaxFactory.BaseList(SyntaxFactory.SeparatedList(new List<BaseTypeSyntax>
            {
                SyntaxFactory.SimpleBaseType(baseTypeSyntax)
            })));

        string? contextLastTypeName = context.LastTypeName;

        if (contextLastTypeName is not null)
        {
            var parentField = SyntaxFactory.FieldDeclaration(SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName(contextLastTypeName))
                                                                          .AddVariables(SyntaxFactory.VariableDeclarator("parent")))
                                           .AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword), SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));

            var ctorSyntax = SyntaxFactory.ConstructorDeclaration(anonTypeName)
                                          .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                                          .AddParameterListParameters(SyntaxFactory.Parameter(SyntaxFactory.ParseToken("parent")).WithType(SyntaxFactory.ParseTypeName(contextLastTypeName)))
                                          .AddBodyStatements(SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, SyntaxFactory.ThisExpression(), SyntaxFactory.IdentifierName("parent")), SyntaxFactory.IdentifierName("parent"))));

            classSyntax = classSyntax.AddMembers(ctorSyntax, parentField);
        }

        // Pass the base type as extends so that @Override methods get the override keyword
        // But only if it's likely a class, not an interface (interfaces don't need override)
        var baseType = newExpr.getType();
        var baseTypeName = baseType.getNameAsString();

        // Heuristic: common interface/class patterns that don't need override in C#
        // - Interfaces: Callback, Runnable, *Listener, *Handler, *Callback, etc.
        // - Java-specific classes: Thread (C# Thread doesn't have overridable Run)
        // Note: Exact match "Listener" is often a nested abstract class (e.g., CellSelector.Listener),
        // so only match *Listener suffix patterns (ActionListener, MouseListener, etc.)
        bool isLikelyInterface = baseTypeName is "Callback" or "Runnable" or "Comparable" or "Comparator" or "Thread"
            || (baseTypeName.EndsWith("Listener") && baseTypeName != "Listener")
            || (baseTypeName.EndsWith("Handler") && baseTypeName != "Handler")
            || (baseTypeName.EndsWith("Observer") && baseTypeName != "Observer")
            || (baseTypeName.EndsWith("Callback") && baseTypeName != "Callback")
            || baseTypeName.EndsWith("able"); // Iterable, Cloneable, etc.

        var extendsList = isLikelyInterface
            ? new List<ClassOrInterfaceType>()
            : new List<ClassOrInterfaceType> { baseType };

        foreach (var memberSyntax in anonBody
                     .Select(member => BodyDeclarationVisitor.VisitBodyDeclarationForClass(context, classSyntax, member, extendsList, new List<ClassOrInterfaceType>()))
                     .OfType<MemberDeclarationSyntax>())
        {
            classSyntax = classSyntax.AddMembers(memberSyntax);
        }

        context.PendingAnonymousTypes.Enqueue(classSyntax);

        var args = newExpr.getArguments();

        if (args is null || args.size() == 0)
        {
            return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseTypeName(anonTypeName))
                .AddArgumentListArguments(SyntaxFactory.Argument(SyntaxFactory.ThisExpression()));
        }

        return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseTypeName(anonTypeName),
            TypeHelper.GetSyntaxFromArguments(context, args),
            null);
    }
}
