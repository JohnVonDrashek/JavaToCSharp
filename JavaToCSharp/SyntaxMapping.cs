namespace JavaToCSharp;

public class SyntaxMapping
{
    public Dictionary<string, string> ImportMappings { get; set; } = new();
    public Dictionary<string, string> TypeMappings { get; set; } = new();
    public Dictionary<string, string> VoidMethodMappings { get; set; } = new();
    public Dictionary<string, string> NonVoidMethodMappings { get; set; } = new();
    public Dictionary<string, string> AnnotationMappings { get; set; } = new();

    /// <summary>
    /// Methods that should be public instead of protected (for override visibility compatibility).
    /// Format: "ClassName.methodName" (Java names, case-sensitive)
    /// </summary>
    public HashSet<string> PublicMethodOverrides { get; set; } = new();

    public static SyntaxMapping Deserialize(string yaml)
    {
        var deserializer = new YamlDotNet.Serialization.Deserializer();
        SyntaxMapping mapping = deserializer.Deserialize<SyntaxMapping>(yaml);
        mapping.Validate();
        return mapping;
    }

    private void Validate()
    {
        // Throw exception if any of the requirements are not meet
        ValidateMethodMapping(VoidMethodMappings);
        ValidateMethodMapping(NonVoidMethodMappings);
    }

    private static void ValidateMethodMapping(Dictionary<string,string> mapping)
    {
        // Throw exception if any of the requirements are not meet
        foreach (string key in mapping.Keys)
        {
            if (key.Contains('.'))
            {
                throw new YamlDotNet.Core.SemanticErrorException("Mappings from fully qualified java methods are not supported");
            }
        }
        foreach (string value in mapping.Values)
        {
            if (string.IsNullOrEmpty(value))
            {
                throw new YamlDotNet.Core.SemanticErrorException("Mappings from java methods can not have an empty value");
            }
        }
    }
}
