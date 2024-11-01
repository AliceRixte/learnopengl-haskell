#version 330 core

in vec4 vertex_color;
in vec2 TexCoord;

out vec4 FragColor;

uniform vec3 objectColor;
uniform vec3 lightColor;
uniform sampler2D texture0;
uniform sampler2D texture1;


void main()
{
    FragColor = vec4(lightColor * objectColor, 1.0);
    // FragColor = mix(texture(texture0, TexCoord), texture(texture1, TexCoord), 0.2);
}
