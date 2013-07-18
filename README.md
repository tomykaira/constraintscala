# ConstraintScala and Uchronie

This repository hosts ConstraintScala and Uchronie.

## ConstraintScala

This is migration of [ConstraintJS](http://cjs.from.so) to Scala + Swing environment.

ConstraintScala provides `Constraint` and `FSM`.

`Constraint` is an object represents a variable thing on GUI, such as the value of slider.

`FSM` manages states and transitions between them.
States in the design can be directly converted to codes.

These objects make GUI design simple and coding easy.
Say good-bye to callback-hell, go to the data-centric, declarative GUI programming.

### How to try

Checkout this project, open `sbt`, and type following commands.

    > project constraintscala
    [info] Set current project to ConstraintScala (in build file:/tmp/constraintscala/)
    > run

    Multiple main classes detected, select one to run:

     [1] io.github.tomykaira.constraintscala.demo.OtherSquare
     [2] io.github.tomykaira.constraintscala.demo.Slider
     [3] io.github.tomykaira.constraintscala.demo.Console

    Enter number:

Choose preferred one.

- OtherSquare: Copy of Constraint.JS Demo program.
- Slider: Copy of Constraint.JS Demo program. This is described in [ConstraintJS - YouTube](http://www.youtube.com/watch?v=193BhlXF0nE).
- Console: My original application. Simple two-pane shell program. Sample of callback management.

### Maven repository

Now preparing...

## Uchronie

Uchronie is a tool to restructure git history.
The idea comes from `git rebase -i` (interactive rebase).

Git's interactive rebase is not friendly and easy to make mistakes.
This GUI tool is truly interactive and safe.

[uchronie - YouTube](https://www.youtube.com/watch?v=GS2cTZeG17k)

It is recommended to have enough git knowledge, but the required amount is smaller than that of `git rebase -i`.

# Contribution

I strongly need your contribution.
I am a beginner of Scala. The code must have many points to fix.

Any comments, issue reports, and pull-requests are welcome.

On reporting issue, please attach the detailed procedure and exception stack trace.

On sending pull-request, add test for new or fixed features.
