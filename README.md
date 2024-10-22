# Learn OpenGL with Haskell

I'm learning OpenGL using Joey de Vries's excellent book [Learn OpenGL](https://learnopengl.com/), except I'm using Haskell as a programming language and SDL2 instead of GLFW. The code follows the book examples and exercices.

# Installing SDL2 for Haskell on Windows

The README from the Haskell's [SDL2](https://github.com/haskell-game/sdl2https://github.com/haskell-game/sdl2) library tells to use pacman to install library.

But there is an [issue](https://github.com/haskell-game/sdl2/issues/277) with the following [workaround](https://github.com/haskell-game/sdl2/issues/277#issuecomment-2283057736).

Another [issue](https://github.com/haskell-game/sdl2/issues/86) is that when importing SDL, printing to the console fails. A [workaround](https://github.com/fjvallarino/monomer/issues/126#issuecomment-1111699512) is to add

```yaml
when:
  - condition: os(windows)
    ghc-options: -optl-mconsole
```

to `package.yaml`
