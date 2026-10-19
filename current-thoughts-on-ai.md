# My current thoughts on AI - 2026-10-18

I kept feeling really lost about all this AI stuff, and I wished that someone knowledgeable would have a take on AI and explain it to me. Like, ideally someone that's a professional software engineer, maybe with a degree in the field, with lots of experience, say at least 10 or 15 years. And then I realize that I'm describing myself. So in this case, I don't have someone else to do the thinking for me (foreshadowing!) and so I felt compelled to collect my thoughts. Consider it my recommendation on how to thrive in the age of AI. I'll start with a summary of my current thinking: AI is a tool that enables you to trade quality for speed.

## What is the value proposition of AI?

All the dialogue and news headlines and anecdotes and buzz about AI successes are focused solely on the speed at which artifacts (e.g. source code, technical docs, prose) are produced. The majority of AI users are trying to integrate AI into their daily workflows with the hope that it will enable them to get their work done faster. It seems that the value proposition of AI is implicit, as I haven't heard anyone lay out the problem that AI is solving, how it solves it, and what the tradeoffs are. I'll start by making the implicit value proposition of AI explicit, as I see it.

From my own firsthand experience plus the firsthand experiences of others, I've concluded that AI excels at producing a large volume of low quality artifacts very quickly. Since the quality of artifacts produced by AI is lower than those produced by humans, it's expected for us to spend some time editing or cleaning up AI artifacts to improve their quality until we consider them to be "good enough". The hope is that the time we spend on editing/cleanup to bring the quality up to "good enough" is less than the time we saved by having AI do the work instead of us.

(Diagram: doing the work before and after AI. Before, single bar with one color. After, work is half the way, and "cleanup" is part of the remainder of the bar)

This creates a problematic dynamic where the work is "done" for some defintion of done and it's then left to our discretion to decide how long to spend on editing/cleanup and what "good enough" means. Given that we're all busy professionals, I don't need to tell you that the result of this is a flexible defintion of "good enough" and many corners being cut. This dynamic has further been complicated by implicit changes to our shared social contract. If I were to put that into words it would be: "it's now fashionable to experiment with shipping lower-quality work, as long as it was created quickly by AI". I'm not sure why I see people are adjusting their standards based on the provenance of the artifact.

## Why quality is important

Quality is not something you merely tack on at the end of a task or project, like a coat of paint. It's an emergent property of a system that results from the time and care a human spends designing and building that system. It's not one thing you can point at; it's a combination of tangible and intangible properties that combine to form something greater than the sum of its parts. I have more to say on this topic, but I believe that the simplest and most deterministic way to lower the cost of building software systems on any timescale is to focus on building high-quality artifacts. High quality code is easier to read, understand, and change. High quality documentation makes it easier to understand how a system works and why it works that way. Quality is the best way to lower your long-term software development costs, in both time and money.

One approach to the quality problem is to turn hard in the other direction: instead of trying to edit/cleanup AI artifacts, what if we just give up not only on writing code but on reading it as well? I understand the appeal; code that was written by someone or something else is much harder to understand than code you wrote yourself. This is because having a mental model of the system you're trying to build is a prerequisite for writing the code yourself. When you read code someone or something else wrote, you have to build that mental model from scratch. AI has created a novel situation where it's now possible to produce working software before understanding it. Some people in our industry are celebrating this, others are mourning it.

If you are using AI to generate code that you don't understand, you're not developing software you're speedrunning tech debt. Doubly so if you choose not to review AI-generated code. This brings me to what I would like to propose as the First Law of AI:

First Law of AI: Understanding is not optional

I'm shocked that anyone is taking seriously the idea that it's acceptable to not read and understand AI-generated code. No technological advancement has ever or will ever remove the need for the people building software systems to understand how those systems work. Software is the most complex thing that humans build. Just because understanding has become the bottleneck doesn't mean it's ethically justified to experiment with skipping it entirely.

## The paradox at the heart of AI

We now have this amazing tool that is powerful enough to fill in the blanks for us when we specify tasks to be completed. However, if something external to us is making those decisions of what to fill in those blanks, then by definition we're not the ones doing it. Since making decisions requires thinking, if we're no longer making those decisions, that means that the thinking is no longer required. We now live in a world where novices and professional software developers alike can produce working software systems without understanding them or doing the work of thinking.

Just because you can do something, doesn't necessarily mean that you should. That's why I propose the Second Law of AI:

Second Law of AI: Don't delegate your thinking to the machine

The practice of reflecting on your own thoughts and ideas develops the skills of self-awareness, discernment, critical thinking, and problem solving. These are the ingredients of being not only a good employee but also a good citizen. The cost of using an AI is more than monetary; delegating your work deprives you of learning and growing. That brings us to my final proposed Law:

Third Law of AI: You should use AI to improve your skills

I'm surprised to see how many people have forgotten that AI is only software. Software is made by humans for the purpose of serving humans. It has recently become fashionable to experiment with discarding long-held beliefs about building software systems. We've recently gotten very good at writing code that writes code, so good in fact that some people out there have begun experimenting with removing humans from the process entirely. I'm curious to see the results and I hope that at the end of this period of experimentation, we have an even stronger sense of what's important and why.
