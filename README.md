
# Calendar Systems

## Project still alpha

I just finished the first version. I could prove that the system works. Now the real works starts
+ Create standard libraries
+ provide documentation
+ specify dsl syntax
+ create tools to create calendar systems
+ tests of course ;)

## Introduction

This project concentrates on creating a library to create and using calendar systems.
it is mainly targeted on performance, correctness and readabillity.
It's possible to create your own calendar-systems that can be transformed in all other calendar system.
We use a reference calendar for transformation.
This makes it possible to use your calendar system with all other calendar systems.
All transformations will be made implicite which result in a pretty readable syntax.

For examle you defined the Mayan Calendar
> Mayan(1,2,3,4,5) + Month(1)

will work without defining what + Month is. (Mayan did not know the Month)


## Goals

1. Creation of Calendar systems that are as correct as possible.
2. Mapping routes between calendar systems during compiletime.
3. Large set of calendar systems
4. Configurable transformation core to controll the precision of transformation.

