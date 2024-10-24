# blockr.workshop (R in Pharma 2024)

## What is block and why you should care?

Despite widespread adoption of tools such as Shiny and dplyr, creating dashboards in R remains challenging for non-coding developers. Solutions like __PowerBI__ or __Tableau__, while popular, are __proprietary__ and __expensive__, offer limited reproducibility, and have restricted integration with the R ecosystem. To address this gap, we introduce blockr, an __open-source__ dashboard builder (https://github.com/blockr-org). blockr simplifies the construction of __parameterised__ data pipelines as a web application, enabling __collaboration__ and easy sharing of results through self-contained code generation. We emphasize __modularity__ by decomposing pipelines into 'blocks'. These 'blocks' can be assembled into 'stacks', which can be connected so that the output of one stack serves as the input for others. This architecture results in a reactive dashboard that allows upstream changes to cascade through the analysis, providing instant feedback to the user. General-purpose blocks provided by blockr can be combined with user-created blocks to __expand functionality__ to use-case specific needs. This approach makes it easy to leverage the vast and mature ecosystem of R packages.

## Prerequisites

This workshop is made for blockr __users without coding experience__ (first part), and blockr __developers with coding experience__ (first and second part).

	- Users: The first part of the workshop does not require any coding experience.
	- Developers: experience in R programming (knowledge about S3: https://adv-r.hadley.nz/s3.html and metaprogramming: http://adv-r.had.co.nz/, will help but are not mandatory), creating R packages.

We actually recommend people to __create groups__ with at least a user and a developer. Each group will go to a __breakout room__ during the exercises. John, Karma and David will check out each room one by one.

## Program (CEST Europe time)

| Time | Activity |                                                    
|----------|----------------------------------------|
| 15:00-15:05 | 👋 [Welcome](./slides/0-welcome/welcome.html) |
| 15:15-16:15 | 📚 [blockr as an end-user](./slides/1-blockr-user/blockr-user.html) |
| 16:15-16:30 | ☕ Break |
| 16:30-17:45 | 📚 [blockr as a developer](./slides/2-blockr-dev/blockr-dev.html) |
| 17:45-18:00 | 🎁 Wrap up |


### Detailed schedule

- Introduction to blockr (1h30):

    - Data analysis is like __cooking__!
    - Definition of fields (__ingredients__), blocks (__instructions__), stack (__cooking recipie__), workspace (__dinner party__).
    - Exercise: create your first stack interactively: some data + plots.
    - Exercise: Use the previously created stack and connect it to anothe   within the workspace.

- Your first new block (1h30):

	- Intro about __blocks constructors__: new_block.
	- Exercise: Develop a clinical table block.
	- Introduction to the __registry, the blocks “supermarket"__.
	- Exercise: use the registry to expose this block to the outside world.
	- Leverage S3 to customise part of the UI/server.
	- Exercise: Add a new render method to display interactive tables.
