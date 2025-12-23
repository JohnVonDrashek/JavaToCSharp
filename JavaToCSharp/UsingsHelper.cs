using com.github.javaparser.ast;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace JavaToCSharp;

public static class UsingsHelper
{
    public static IEnumerable<UsingDirectiveSyntax> GetUsings(ConversionContext context,
        IEnumerable<ImportDeclaration> imports,
        JavaConversionOptions? options,
        NameSyntax? namespaceNameSyntax)
    {
        var usings = new List<UsingDirectiveSyntax>();

        foreach (var import in imports)
        {
            // The import directive in Java will import a specific class.
            string importName = import.getNameAsString();

            // Strip the imported class name
            var lastDotIndex = importName.LastIndexOf('.');
            if (lastDotIndex == -1)
                continue; // Single-segment import, skip

            var packagePath = importName[..lastDotIndex];

            // Keep stripping while the last segment is capitalized (a class, not a package)
            // Java convention: packages are lowercase, classes are PascalCase
            while (true)
            {
                lastDotIndex = packagePath.LastIndexOf('.');
                if (lastDotIndex == -1)
                    break; // Only one segment left, use it

                var lastSegment = packagePath[(lastDotIndex + 1)..];
                if (lastSegment.Length > 0 && char.IsUpper(lastSegment[0]))
                {
                    // Last segment is a class name, strip it
                    packagePath = packagePath[..lastDotIndex];
                }
                else
                {
                    break; // Last segment is a package (lowercase), we're done
                }
            }

            var nameSpace = TypeHelper.Capitalize(packagePath);

            // Override namespace if a non empty mapping is found (mapping to empty string removes the import)
            if (options is not null && options.SyntaxMappings.ImportMappings.TryGetValue(importName, out var mappedNamespace))
            {
                if (string.IsNullOrEmpty(mappedNamespace))
                {
                    continue;
                }
                nameSpace = mappedNamespace;
            }

            var usingSyntax = SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(nameSpace));

            if (context.Options.IncludeComments)
            {
                usingSyntax = CommentsHelper.AddUsingComments(usingSyntax, import);
            }

            usings.Add(usingSyntax.NormalizeWhitespace().WithTrailingNewLines());
        }

        if (options?.IncludeUsings == true)
        {
            usings.AddRange(options.Usings
                .Where(x => !string.IsNullOrWhiteSpace(x))
                .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(ns)).NormalizeWhitespace().WithTrailingNewLines()));
        }

        if (namespaceNameSyntax is not null)
        {
            foreach (var staticUsing in options?.StaticUsingEnumNames ?? [])
            {
                var usingSyntax = SyntaxFactory
                    .UsingDirective(SyntaxFactory.ParseName($"{namespaceNameSyntax}.{staticUsing}"))
                    .WithStaticKeyword(SyntaxFactory.Token(SyntaxKind.StaticKeyword))
                    .NormalizeWhitespace()
                    .WithTrailingNewLines();

                usings.Add(usingSyntax);
            }
        }

        usings = usings.Distinct(new UsingDirectiveSyntaxComparer()).ToList();

        if (usings.Count > 0)
        {
            usings[^1] = usings[^1].WithTrailingNewLines(2);
        }

        return usings;
    }
}

public class UsingDirectiveSyntaxComparer : IEqualityComparer<UsingDirectiveSyntax>
{
    public bool Equals(UsingDirectiveSyntax? x, UsingDirectiveSyntax? y)
    {
        if (ReferenceEquals(x, y)) return true;
        if (x is null) return false;
        if (y is null) return false;
        if (x.GetType() != y.GetType()) return false;

        return Equals(x.Alias?.ToString(), y.Alias?.ToString()) &&
               Equals(x.Name?.ToString(), y.Name?.ToString());
    }

    public int GetHashCode(UsingDirectiveSyntax obj)
        => HashCode.Combine(obj.Alias?.ToString() ?? "", obj.Name?.ToString() ?? "");
}
