PennController.ResetPrefix(null)
DebugOff()
SetCounter("group", "inc", 1);
newVar("failed-A-1").global().set(0)
newVar("failed-slider-1").global().set(0)
newVar("failed-election").global().set(0)
newVar("failed-A-2").global().set(0)
newVar("failed-slider-2").global().set(0)

// Sequence of the experiment
Sequence("consent", "counter", "instruction-start",
         "instruction-gumball", 
         "instruction-A-1", "check-A-1",
         "instruction-slider-1", "check-slider-1",
         "practice-gumball-start", "practice-gumball", "practice-gumball-end",
         "trials-gumball", 
         "break-1",
         "instruction-unk-election",
         "check-election-2",
         "instruction-A-2", "check-A-2",
         "instruction-slider-2", "check-slider-2",
         "practice-election-start", "practice-election", "practice-election-end",
         "trials-unk-election", 
         "break-2",
         "interpretation-A-instruction", "interpretation-A-trials",
         "interpretation-B-instruction", "interpretation-B-trials",
         "break-3",
         "demographics",
         "send", "completion")
         
// Consent form
newTrial("consent",
    newHtml("consent_form", "consent.html")
        .cssContainer({"width":"720px"})
        .css("font-size", "25px")
        .checkboxWarning("You must consent before continuing.")
        .inputWarning("Please enter your Prolific ID.")
        .print()
        .log()
    ,
    newButton("continue", "Click here to continue")
        .css("font-size", "25px")
        .center()
        .print()
        .wait(
            getHtml("consent_form").test.complete()
                .failure(getHtml("consent_form").warn())
        )
)

// Instruction start
newTrial("instruction-start",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "Welcome!")
    ,
    newText("i2", "There are three parts in this experiment. In the first two parts, you will be presented with sentences that describe different scenes. A fictional person “A”, who cannot see the scenes, will ask you questions about the scenes. For each of A’s questions, you will also be presented with three possible responses. Your task is to decide how much you prefer each of the three utterances as a response to A’s questions.")
    ,
    newText("i3", "In the third part, you will be presented with images without the fictional person “A”, and your task is to answer questions about the images.")
    ,
    newText("br", "")
    ,
    newButton("wait", "Click to start")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)

// Gumball explanation
newTrial("instruction-gumball",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "In this part of the experiment, you will be presented with sentences that describe gumball machines.")
    ,
    newText("i2", "The gumball machines are filled with <span style='color: #BF40BF'>purple</span> and <span style='color: #FFA500'>orange</span> gumballs. The gumballs will be tossed around before a random one is dispensed. Here is an example of how a gumball machine may be described.")
    ,
    newText("br", "")
    ,
    newText("example", "-------------------------------------------<br>SCENE<br>You see a gumball machine with: <br>  • 60% of <span style='color: #BF40BF'>purple</span> gumballs<br>  • 40% of <span style='color: #FFA500'>orange</span> gumballs<br>-------------------------------------------")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)


// A explanation (Block 1)
newTrial("instruction-A-1",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    getVar("failed-A-1")
        .test.is(1)
            .success(
                newText("retry-1", "It seems like you misunderstood something...")
                ,
                newText("retry-2", "<b>Please read the instructions again before returning to the experiment.</b>")
                ,
                newText("br", "")
                )
    ,
    newText("i1", "In this experiment, there is also a fictional person “A”.")
    ,
    newText("i2", "The fictional person “A”, <b>who is in a different room, and cannot see the gumball machines</b>, will ask you questions about the gumball machines.")
    ,
    newText("i3", "Here is an example of what A's question may look like.")
    ,
    newText("example", "-------------------------------------------<br>QUESTION<br><b>A</b>: Will I get a <span style='color: #BF40BF'>purple</span> gumball?<br>-------------------------------------------")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)


// // Check whether participants perceive A as L0 (block 1)
newTrial("check-A-1",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("example", "SCENE<br>You see a gumball machine with: <br>  • 60% of <span style='color: #BF40BF'>purple</span> gumballs<br>  • 40% of <span style='color: #FFA500'>orange</span> gumballs")
    ,
    newText("A", "-------------------------------------------<br>QUESTION<br><b>A</b>: Will I get a <span style='color: #BF40BF'>purple</span> gumball?<br>-------------------------------------------")
    ,
    newText("question", "In the scenario above, can A see the gumballs in the machine?")
    ,
    newText("br", "")
    ,
    newButton("go-back", "Yes")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .callback(
            getVar("failed-A-1").set(1),
            jump("instruction-A-1"), end()
        )
        .log()
    ,
    newButton("proceed", "No")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .log()
    ,
    getButton("proceed")
        .wait()
)

// Instructions for slider (block 1)
newTrial("instruction-slider-1",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    getVar("failed-slider-1")
        .test.is(1)
            .success(
                newText("retry-1", "It seems like you misunderstood something...")
                ,
                newText("retry-2", "<b>Please read the instructions again before returning to the experiment.</b>")
                ,
                newText("br", "")
                )
    ,
    newText("i1", "In the experiment, you will be presented with three possible responses for each of A’s questions.")
    ,
    newText("i2", "For each response, you will use a slider to indicate your degree of preference for the response. Each slider will start at the far left, which indicates zero preference. If you would never use an utterance to answer A’s question, leave its slider at the far left. If you would always pick an utterance over the other two utterances, move its slider to the far right and leave the other two sliders in their original positions. If all the sliders are set to the same position, it means you are equally likely to pick any of the utterances to answer A’s question.")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)

// Check sliders understanding (block 1)
newTrial("check-slider-1",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newImage("left", "sliders_left.png")
        .size(200, 200)
    ,
    newImage("mid", "sliders_mid.png")
        .size(200, 200)
    ,
    newText("question", "Do the slider positions in the two images reflect the same relative preference among the three responses?")
    ,
    newCanvas("sliders", 400, 200)
        .center()
        .add(0, 0, getImage("left"))
        .add(200, 0, getImage("mid"))
        .print()
    ,
    newText("br", "")
    ,
    newButton("go-back", "No")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .callback(
            getVar("failed-slider-1").set(1),
            jump("instruction-slider-1"), end()
        )
    ,
    newButton("proceed", "Yes")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .log()
    ,
    getButton("proceed")
        .wait()
)

// Practice start
newTrial("practice-gumball-start",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished all the instructions for this part.")
    ,
    newText("i2", "Let us try out a few practice trials. Please click the button below to start the practice.")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)


// Practice trials
Template("practice_gumball_text.csv", row =>
    newTrial("practice-gumball",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("context", row.context)
        ,
        ( row.type=="experimental-trial" ? [
                newText("content", row.content)
                ,
                newText("question", row.question)
                ,
                newText("instruction", row.instruction)
                ,
                newScale("slider-1", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-2", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-3", 101)
                    .slider()
                    .default(0)
                ,
                newCanvas("sliders", 600, 120)
                    .center()
                    .add(0, 10, newText("probably", row.probably))
                    .add(0, 40, newText("might", row.might))
                    .add(0, 70, newText("bare", row.bare))
                    .add(400, 10, getScale("slider-1"))
                    .add(400, 40, getScale("slider-2"))
                    .add(400, 70, getScale("slider-3"))
                    .print()
                ,
                newButton("wait", "Click to continue")
                    .css("font-size", "25px")
                    .center()
                    .print()
                    .disable()
                ,
                getScale("slider-1")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-2")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-3")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
                ,
                getScale("slider-1").log()
                ,
                getScale("slider-2").log()
                ,
                getScale("slider-3").log()
            ] : [
                newText("correct", row.correct)
                ,
                newText("incorrect", row.incorrect)
                ,
                newSelector("selection")
                    .add(getText("correct"), getText("incorrect"))
                    .shuffle()
                    .log()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getSelector("selection")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
            ] )
    )
    .log("id", row.id)
    .log("group", row.group)
)

// End of practice instruction page
newTrial("practice-gumball-end",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished the practice.")
    ,
    newText("i2", "Please click the button below to start the first part of the experiment.")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Block 1: gumball trials
Template("lists_gumball_text.csv", row =>
    newTrial("trials-gumball",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("context", row.context)
        ,
        ( row.type=="experimental-trial" ? [
                newText("content", row.content)
                ,
                newText("question", row.question)
                ,
                newText("instruction", row.instruction)
                ,
                newScale("slider-1", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-2", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-3", 101)
                    .slider()
                    .default(0)
                ,
                newCanvas("sliders", 600, 120)
                    .center()
                    .add(0, 10, newText("probably", row.probably))
                    .add(0, 40, newText("might", row.might))
                    .add(0, 70, newText("bare", row.bare))
                    .add(400, 10, getScale("slider-1"))
                    .add(400, 40, getScale("slider-2"))
                    .add(400, 70, getScale("slider-3"))
                    .print()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getScale("slider-1")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-2")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-3")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
                ,
                getScale("slider-1").log()
                ,
                getScale("slider-2").log()
                ,
                getScale("slider-3").log()
            ] : [
                newText("correct", row.correct)
                ,
                newText("incorrect", row.incorrect)
                ,
                newSelector("selection")
                    .add(getText("correct"), getText("incorrect"))
                    .shuffle()
                    .log()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getSelector("selection")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
            ] )
    )
    .log("id", row.id)
    .log("group", row.group)
)

// End of block 1
newTrial("break-1",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished the first part of the experiment!")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)


// instructions for unk election
newTrial("instruction-unk-election",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    getVar("failed-election")
        .test.is(1)
            .success(
                newText("retry-1", "Remember that the prediction does not represent the percentage of votes a party is expected to get in a certain region, but the chance of winning the region. If a party has a 90% chance of winning a region, then 90 times out of 100, it will get more than 50% of the votes, hence winning the region.")
                ,
                newText("retry-2", "<b>Please read the instructions again before returning to the experiment.</b>")
                ,
                newText("br", "")
                )
    ,
    newText("i1", "In this part of the experiment, you will be presented with sentences that describe election predictions from an unknown country.")
    ,
    newText("i2", "In this country, the two major parties, <span style='color: #BF40BF'>Party X</span> and <span style='color: #FFA500'>Party Y</span>, compete for votes in different regions. In each region, the party with the maximum votes wins that region.")
    ,
    newText("i3", "A bipartisan company is interested in studying the probability of the two parties winning in different parts of the country – specifically they are interested in comparing the results in different regions. They generate predictions about the outcomes of the elections region by region. This company has a great track record of generating <b>very reliable</b> predictions.")
    ,
    newText("i4", "If the company said a certain party had a 90% chance of winning in a certain region , then 90 times out of 100 that party would actually win in that region. Similarly, if they said that there was a 40% chance of a party winning in a certain region, then only 40 times out of 100 that party would win in that region.")
    ,
    newText("i5", "According to the prediction below, <span style='color: #BF40BF'>Party X</span> will win 50 times out of 100 in this region, and <span style='color: #FFA500'>Party Y</span> will win 50 times out of 100 in this region.")
    ,
    newText("example", "-------------------------------------------<br>SCENE<br>You see the company predicts: <br>  • 50% chance of <span style='color: #BF40BF'>Party X</span> winning<br>  • 50% chance of <span style='color: #FFA500'>Party Y</span> winning<br>-------------------------------------------")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)

// Check election understanding (Block 2 only)
newTrial("check-election-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("check", "-------------------------------------------<br>SCENE<br>You see the company predicts: <br>  • 70% chance of <span style='color: #BF40BF'>Party X</span> winning<br>  • 30% chance of <span style='color: #FFA500'>Party Y</span> winning<br>-------------------------------------------")
    ,
    newText("question", "In this scene above, does the election company predict that <span style='color: #BF40BF'>Party X</span> will get 70% of the votes?")
    ,
    newText("br", "")
    ,
    newButton("go-back", "Yes")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .callback(
            getVar("failed-election").set(1),
            jump("instruction-unk-election"), end()
        )
        .log()
    ,
    newButton("proceed", "No")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .log()
    ,
    getButton("proceed")
        .wait()
)


// A explanation (Block 2)
newTrial("instruction-A-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    getVar("failed-A-2")
        .test.is(1)
            .success(
                newText("retry-1", "It seems like you misunderstood something...")
                ,
                newText("retry-2", "<b>Please read the instructions again before returning to the experiment.</b>")
                ,
                newText("br", "")
                )
    ,
    newText("i1", "In this experiment, there is also a fictional person “A”.")
    ,
    newText("i2", "The fictional person “A”, <b>who is in a different room, and cannot see the election predictions</b>, will ask you questions about the election predictions.")
    ,
    newText("i3", "Here is an example of what A's question may look like.")
    ,
    newText("example", "-------------------------------------------<br>QUESTION<br><b>A</b>: Will <span style='color: #BF40BF'>Party X</span> win the election?<br>-------------------------------------------")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Check A understanding (block 2)
newTrial("check-A-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
   newText("example", "SCENE<br>You see the company predicts: <br>  • 60% chance of <span style='color: #BF40BF'>Party X</span> winning<br>  • 40% chance of <span style='color: #FFA500'>Party Y</span> winning")
    ,
    newText("A", "-------------------------------------------<br>QUESTION<br><b>A</b>: Will <span style='color: #BF40BF'>Party X</span> win the election?<br>-------------------------------------------")
    ,
    newText("question", "In the scenario above, can A see the election prediction?")
    ,
    newText("br", "")
    ,
    newButton("go-back", "Yes")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .callback(
            getVar("failed-A-2").set(1),
            jump("instruction-A-2"), end()
        )
        .log()
    ,
    newButton("proceed", "No")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .log()
    ,
    getButton("proceed")
        .wait()
)

// Instructions for slider (block 2)
newTrial("instruction-slider-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    getVar("failed-slider-2")
        .test.is(1)
            .success(
                newText("retry-1", "It seems like you misunderstood something...")
                ,
                newText("retry-2", "<b>Please read the instructions again before returning to the experiment.</b>")
                ,
                newText("br", "")
                )
    ,
    newText("i1", "In the experiment, you will be presented with three possible responses for each of A’s questions.")
    ,
    newText("i2", "For each response, you will use a slider to indicate your degree of preference for the response. Each slider will start at the far left, which indicates zero preference. If you would never use an utterance to answer A’s question, leave its slider at the far left. If you would always pick an utterance over the other two utterances, move its slider to the far right and leave the other two sliders in their original positions. If all the sliders are set to the same position, it means you are equally likely to pick any of the utterances to answer A’s question.")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Check sliders understanding (block 2)
newTrial("check-slider-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newImage("left", "sliders_left.png")
        .size(200, 200)
    ,
    newImage("mid", "sliders_mid.png")
        .size(200, 200)
    ,
    newText("question", "Do the slider positions in the two images reflect the same relative preference among the three responses?")
    ,
    newCanvas("sliders", 400, 200)
        .center()
        .add(0, 0, getImage("left"))
        .add(200, 0, getImage("mid"))
        .print()
    ,
    newText("br", "")
    ,
    newButton("go-back", "No")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .callback(
            getVar("failed-slider-2").set(1),
            jump("instruction-slider-2"), end()
        )
    ,
    newButton("proceed", "Yes")
        .center()
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .print()
        .log()
    ,
    getButton("proceed")
        .wait()
)

// Practice start
newTrial("practice-election-start",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished all the instructions for this part.")
    ,
    newText("i2", "Let us try out a few practice trials. Please click the button below to start the practice.")
    ,
    newButton("wait", "Click to proceed")
        .css("font-size", "25px")
        .center()
        .print()
        .wait()
)


// Practice trials
Template("practice_election_text.csv", row =>
    newTrial("practice-election",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("context", row.context)
        ,
        ( row.type=="experimental-trial" ? [
                newText("content", row.content)
                ,
                newText("question", row.question)
                ,
                newText("instruction", row.instruction)
                ,
                newScale("slider-1", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-2", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-3", 101)
                    .slider()
                    .default(0)
                ,
                newCanvas("sliders", 600, 120)
                    .center()
                    .add(0, 10, newText("probably", row.probably))
                    .add(0, 40, newText("might", row.might))
                    .add(0, 70, newText("bare", row.bare))
                    .add(400, 10, getScale("slider-1"))
                    .add(400, 40, getScale("slider-2"))
                    .add(400, 70, getScale("slider-3"))
                    .print()
                ,
                newButton("wait", "Click to continue")
                    .css("font-size", "25px")
                    .center()
                    .print()
                    .disable()
                ,
                getScale("slider-1")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-2")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-3")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
                ,
                getScale("slider-1").log()
                ,
                getScale("slider-2").log()
                ,
                getScale("slider-3").log()
            ] : [
                newText("correct", row.correct)
                ,
                newText("incorrect", row.incorrect)
                ,
                newSelector("selection")
                    .add(getText("correct"), getText("incorrect"))
                    .shuffle()
                    .log()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getSelector("selection")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
            ] )
    )
    .log("id", row.id)
    .log("group", row.group)
)

// End of practice instruction page
newTrial("practice-election-end",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished the practice.")
    ,
    newText("i2", "Please click the button below to start the first part of the experiment.")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Block 2: unk election trials
Template("lists_election_text.csv", row =>
    newTrial("trials-unk-election",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("context", row.context)
        ,
        ( row.type=="experimental-trial" ? [
                newText("content", row.content)
                ,
                newText("question", row.question)
                ,
                newText("instruction", row.instruction)
                ,
                newScale("slider-1", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-2", 101)
                    .slider()
                    .default(0)
                ,
                newScale("slider-3", 101)
                    .slider()
                    .default(0)
                ,
                newCanvas("sliders", 600, 120)
                    .center()
                    .add(0, 10, newText("probably", row.probably))
                    .add(0, 40, newText("might", row.might))
                    .add(0, 70, newText("bare", row.bare))
                    .add(400, 10, getScale("slider-1"))
                    .add(400, 40, getScale("slider-2"))
                    .add(400, 70, getScale("slider-3"))
                    .print()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getScale("slider-1")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-2")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getScale("slider-3")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
                ,
                getScale("slider-1").log()
                ,
                getScale("slider-2").log()
                ,
                getScale("slider-3").log()
            ] : [
                newText("correct", row.correct)
                ,
                newText("incorrect", row.incorrect)
                ,
                newSelector("selection")
                    .add(getText("correct"), getText("incorrect"))
                    .shuffle()
                    .log()
                ,
                newButton("wait", "Click to continue")
                    .center()
                    .css("font-size", "25px")
                    .print()
                    .disable()
                ,
                getSelector("selection")
                    .callback(
                        getButton("wait").enable()
                    )
                ,
                getButton("wait").wait()
            ] )
    )
    .log("id", row.id)
    .log("group", row.group)
)


// End of block 2
newTrial("break-2",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("i1", "You have finished the second part of the experiment!")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Interpretation phase

// Interpretation of gumball
Template("instruction_interpretation_A_text.csv", row =>
    newTrial("interpretation-A-instruction",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("i1", row.instruction)
        ,
        newText("br", "")
        ,
        newButton("wait", "Click to proceed")
            .center()
            .css("font-size", "25px")
            .print()
            .wait()
    )
)

// Gumball interpretation trials
Template("interpretation_A_text.csv", row =>
    newTrial("interpretation-A-trials",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("image", row.content)
        ,
        newText("question", row.question)
        ,
        newScale("interpretation", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
            .labelsPosition("top")
            .checkbox()
            .center()
            .print()
        ,
        newText("br", "")
        ,
        newButton("wait", "Click to continue")
            .center()
            .css("font-size", "25px")
            .print()
            .disable()
        ,
        getScale("interpretation")
            .callback(
                getButton("wait").enable()
            )
        ,
        getButton("wait", "Click to proceed")
            .wait()
        ,
        getScale("interpretation")
            .log()
    )
    .log("id", row.id)
    .log("group", row.group)
)

// Interpretation of election 
Template("instruction_interpretation_B_text.csv", row =>
    newTrial("interpretation-B-instruction",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("i1", row.instruction)
        ,
        newText("br", "")
        ,
        newButton("wait", "Click to proceed")
            .center()
            .css("font-size", "25px")
            .print()
            .wait()
    )
)


// Election interpretation trials
Template("interpretation_B_text.csv", row =>
    newTrial("interpretation-B-trials",
        defaultText
            .cssContainer({"margin-bottom":"1em"})
            .css("font-size", "25px")
            .center()
            .print()
        ,
        newText("image", row.content)
        ,
        newText("question", row.question)
        ,
        newScale("interpretation", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
            .labelsPosition("top")
            .checkbox()
            .center()
            .print()
        ,
        newText("br", "")
        ,
        newButton("wait", "Click to continue")
            .center()
            .css("font-size", "25px")
            .print()
            .disable()
        ,
        getScale("interpretation")
            .callback(
                getButton("wait").enable()
            )
        ,
        getButton("wait", "Click to proceed")
            .wait()
        ,
        getScale("interpretation")
            .log()
    )
    .log("id", row.id)
    .log("group", row.group)
)


// End of experimental trials instruction page
newTrial("break-3",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("intruction-1", "Almost there!")
    ,
    newText("intruction-2", "Please fill out the demographic survey on the next page. Please answer <b>all</b> of the questions on the survey. ")
    ,
    newText("instruction-3", "Remember, no one other than the researchers will be able to link your data or responses to the demographic survey to your Prolific ID. Before making any of this data publicly available, we will replace your Prolific ID with a random ID.")
    ,
    newButton("wait", "Click to proceed")
        .center()
        .css("font-size", "25px")
        .print()
        .wait()
)

// Demographic survey
newTrial("demographics",
    newHtml("demographics_survey", "demographics.html")
        .cssContainer({"width":"720px"})
        .css("font-size", "25px")
        .inputWarning("You haven't completed the survey yet. ")
        .radioWarning("You haven't completed the survey yet. ")
        .print()
        .log()
    ,
    newButton("continue", "Click here to continue")
        .css("font-size", "25px")
        .center()
        .print()
        .wait(
            getHtml("demographics_survey").test.complete()
                .failure(getHtml("demographics_survey").warn())
        )
)

SendResults("send")

newTrial("completion",
    defaultText
        .cssContainer({"margin-bottom":"1em"})
        .css("font-size", "25px")
        .center()
        .print()
    ,
    newText("thanks", "Thank you for participating! Your submission on Prolific will be approved within 5 business days.")
    ,
    newText("code", "Please save your completion code: <b>C1M2WBNY</b>")
    ,
    newText("exit", "You may exit the window now.")
    ,
    newButton("void", "")
        .wait()
)
