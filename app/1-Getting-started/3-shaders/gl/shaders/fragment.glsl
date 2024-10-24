#version 330 core

in vec4 vertex_color;
out vec4 FragColor;

uniform vec4 ourColor;

void main()
{
    FragColor = vertex_color; // vec4(1.0f, 0.5f, 0.2f, 1.0f);
}