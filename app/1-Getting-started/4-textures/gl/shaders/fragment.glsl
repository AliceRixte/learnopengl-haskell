#version 330 core

in vec4 vertex_color;
in vec2 TexCoord;

out vec4 FragColor;

uniform vec4 ourColor;
uniform sampler2D texture0;
uniform sampler2D texture1;

void main()
{
    vec2 blub = vec2(TexCoord.t, TexCoord.s);
    FragColor = mix(texture(texture0, TexCoord), texture(texture1, blub), 0.2);
}