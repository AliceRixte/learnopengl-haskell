#version 330 core

in vec2 TexCoords;
in vec3 Normal;
in vec3 FragPos;

out vec4 FragColor;

struct Material {
    sampler2D diffuse;
    sampler2D specular;
    float shininess;
};


struct PointLight {
    vec3 position;

    vec3 ambient;
    vec3 diffuse;
    vec3 specular;

    float constant;
    float linear;
    float quadratic;
};

struct DirLight {
    vec3 direction;

    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
};

struct SpotLight {
    vec3 position;
    vec3 direction;

    float cutOff;
    float outerCutOff;

    vec3 ambient;
    vec3 diffuse;
    vec3 specular;

    float constant;
    float linear;
    float quadratic;
};

#define NB_POINT_LIGHTS 4

uniform Material material;
uniform PointLight pointLights[NB_POINT_LIGHTS];
uniform DirLight dirLight;
uniform SpotLight spotLight;

uniform sampler2D texture0;
uniform sampler2D texture1;
uniform vec3 viewPos;

vec3 pointLightContrib(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir);
vec3 spotLightContrib(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir);
vec3 dirLightContrib(DirLight light, vec3 normal, vec3 viewDir);

void main()
{
    vec3 fragColor3 = vec3(0.0);

    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 normal = normalize(Normal);

    for(int i = 0; i < NB_POINT_LIGHTS ; i++){
        fragColor3 +=  pointLightContrib(pointLights[i], normal, FragPos, viewDir);
    }

    fragColor3 += dirLightContrib(dirLight, normal, viewDir);
    // fragColor3 += spotLightContrib(spotLight, normal, FragPos, viewDir);

    FragColor = vec4(fragColor3, 1.0);

}

vec3 pointLightContrib(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir){

    float dist = distance(light.position, FragPos);
    float attenuation = 1 / (light.constant + dist * (light.linear + light.quadratic * dist ));

    vec3 lightDir = normalize(light.position - FragPos);

    float diff = max(dot(normal, lightDir), 0.0);

    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

    vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoords));
    vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, TexCoords));
    vec3 specular = light.specular * spec * vec3(texture(material.specular, TexCoords));

    ambient  *= attenuation;
    diffuse  *= attenuation;
    specular *= attenuation;

    return ambient + diffuse + specular;

}

vec3 dirLightContrib(DirLight light, vec3 normal, vec3 viewDir){

    vec3 lightDir = normalize(-light.direction);

    float diff = max(dot(normal, lightDir), 0.0);

    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

    vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoords));
    vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, TexCoords));
    vec3 specular = light.specular * spec * vec3(texture(material.specular, TexCoords));

    return ambient + diffuse + specular;
}


vec3 spotLightContrib(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir){

    float dist = distance(light.position, FragPos);
    float attenuation = 1 / (light.constant + dist * (light.linear + light.quadratic * dist ));

    vec3 lightDir = normalize(light.position - FragPos);

    float theta = dot(lightDir,normalize(-light.direction));
    float epsilon   = light.cutOff - light.outerCutOff;
    float intensity = clamp((theta - light.outerCutOff) / epsilon, 0.0, 1.0);

    float diff = intensity * max(dot(normal, lightDir), 0.0);

    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

    vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoords));
    vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, TexCoords));
    vec3 specular = light.specular * spec * vec3(texture(material.specular, TexCoords));

    ambient  *= attenuation;
    diffuse  *= attenuation;
    specular *= attenuation;

    return diffuse + specular;

}