# README

Build the Connect 4 game in Haskell using the OpenAI GPT-4 implementation.

## Purpose

The purpose of this exercise is to *explore how easy it is to use GPT-4 (will be called "the AI" in the following text) to partner with you to rapidly prototype a real Haskell implementation of the **[Connect 4](https://en.wikipedia.org/wiki/Connect_Four) game**.*

Initially I intended to try the following discrete AI commands:

1. Generate a Haskell *programmed solution* for **Connect 4**.  What would code look like?
2. Generate a Haskell *module* to create a *console* user interface using the module generated in step 1.
3. Generate a Haskell module to create a *GUI* using the module generated in step 1.
4. Generate a Haskell test file to run unit tests against the module generated in step 1.

## Questions I had before starting

* Would I get a useful response to a very general request for my first AI request?
* Would I be able to iterate easily to add functionality missed by previous AI requests?
* Would there be any `stack` build assistance?
* Would the generated Haskell source file(s) compile?
* Would the program build?
* Would the program launch successfully?
* Would the program behave correctly while running it?
* Were the features representative of what the AI promised?

## Implementation

I decided to start before reading any OpenAI documentation.  Jump feet first.  Just how user friendly is the AI?

### Preparation

1. I signed up for the GPT-3.5 service first, and launched my first effort:
    1. Browsed [Open AI](https://openai.com).  I used **Safari**[^1].
    2. Clicked the button [Learn about GPT-4](https://openai.com/product/gpt-4).
    3. Clicked [Try on ChatGPT Plus](http://chat.openai.com/).
    4. Observed the GPT "chat" window which looks like this:
        ![Initial GPT Chat Window](images/InitialGPTChatWindow.png)
    5.

[^1]: I'll note that Safari wasn't (to my knowledge) responsible for any of the problems I would encounter.
