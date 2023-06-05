let sentences = [];

function generate() {
    var data = {
    	"start": ["#PN# #CN# #TV#.", "#PN# #CN# #TV# #AV# #V# #CNN#."],
    	"PN": ["ici", "uyu", "ulya" ],
    	"CN": ["lumendo", "tata", "lumendo", "mwanakashi" ],
    	"TV": ["alishiba", "taishiba", "Nafuma" ],
    	"AV": ["uku" ],
    	"V": ["shana", "funda", "imba", "lolesha" ],
        "CNN": ["nsamushi", "amafunde", "ulwimbo" ],
    	
    }
    
    let grammar = tracery.createGrammar(data);
    let expansion = grammar.flatten('#start#');

    sentences.push(expansion);

    printSentences(sentences);
}

function printSentences(sentences) {
    let textBox = document.getElementById("sentences");
    textBox.innerHTML = "";
    for(let i=sentences.length-1; i>=0; i--) {
        textBox.innerHTML += "<p>"+sentences[i]+"</p>"
    }
}

