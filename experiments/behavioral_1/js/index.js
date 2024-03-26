//https://www.ncbi.nlm.nih.gov/pmc/articles/doi/10.3389/fpsyg.2014.00399/full// Returns a random integer between min (included) and max (excluded)

// function present_handle(stim) {
//     $("#target_word").show();
//     var self = this;
//     console.log(stim);
//     self.trial_start = Date.now();
//     self.stim = stim;
//     console.log(self.stim);
//     $("#target_word").html(self.stim.item);
//
//     document.onkeypress = checkKey;
//     function checkKey(e) {
//         e = e || window.event;
//         console.log(e);
//         if (e.keyCode == 102 || e.keyCode == 106) {
//             console.log(e.key)
//             button.call(self, e.key, self.stim);
//             console.log(e.keyCode);
//         }
//     }
// }
//
// function button(response, stim) {
//     var self = this;
//     logResponses.call(self, response, stim);
//     _stream.apply(self);
// }
//
// function logResponses(response, stim) {
//     var self = this;
//     console.log(stim.item);
//     console.log(stim.lemma);
//     console.log(response);
//     exp.data_trials.push({
//         "slide_number_in_experiment": exp.phase,
//         "word": stim.item,
//         "lemma": stim.lemma,
//         "response": response,
//         "rt": Date.now() - self.trial_start
//     });
// }



// /*
// Function that shows the stimulus for each trial
// */
// function present_handle(stim) {
//     var self = this; // Store reference to 'this'
//
//     console.log(stim);
//     self.trial_start = Date.now();
//     self.stim = stim;
//     console.log(self.stim);
//     $("#target_word").html(self.stim.item);
//
//     // ENTER TO CONTINUE
//     document.onkeypress = checkKey;
//     function checkKey(e) {
//         e = e || window.event;
//         console.log(e);
//         // 102 = f, 106 = j
//         if (e.keyCode == 102 || e.keyCode == 106) {
//             console.log(e.key)
//             button.call(self, e.key, self.stim); // Explicitly bind 'this' and pass 'stim'
//             console.log(e.keyCode);
//         }
//     }
// }
//
// /*
// Function that moves down the stimuli stream once a /j/ or /f/
// button was successfully pressed during a trial
// */
// function button(response, stim) {
//     var self = this; // Store reference to 'this'
//     logResponses.call(self, response, stim); // Explicitly bind 'this' and pass 'stim'
//     _stream.apply(self);
// }
//
// /*
// Function that logs the responses of each trial.
// */
// function logResponses(response, stim) {
//     var self = this; // Store reference to 'this'
//
//     console.log(stim.item);
//     console.log(stim.lemma);
//     console.log(response);
//     exp.data_trials.push({
//         "slide_number_in_experiment": exp.phase,
//         "word": stim.item,
//         "lemma": stim.lemma,
//         "response": response,
//         "rt": Date.now() - self.trial_start // Access 'self.trial_start' explicitly
//     });
// }


function make_slides(f) {
  var slides = {};

  slides.consent = slide({
     name : "consent",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.practicetrial = slide({
    name: "practicetrial",
    present: exp.prac_stims,
    present_handle: function(stim) {
        $("#feedback").hide();
        $("#feedback").css("color", "red");

        console.log(stim);
        this.trial_start = Date.now();

        this.stim = stim;
        console.log(this.stim);

        $("#practice_word").html(this.stim.word);

        let correctKeyPressed = false; // Flag to track if correct key has been pressed

        document.onkeypress = (e) => {
            if (!correctKeyPressed) { // Check if correct key has not been pressed yet
                checkKey.call(this, e);
            }
        };

        function checkKey(e) {
            e = e || window.event;
            console.log(e);

            // 102 = f (fake word)
            // 106 = j (real word)
            if (e.keyCode != 102 && e.keyCode != 106) {
                $("#feedback").show();
                $("#feedback").html("Por favor pulse la tecla \'j\' o \'f\'");
            } else if (e.keyCode == 102 && this.stim.answer == 1) {
                $("#feedback").show();
                $("#feedback").html("¡" + this.stim.word + " es una palabra real! Por favor pulse la tecla \'j\'");
            } else if (e.keyCode == 106 && this.stim.answer == 0) {
                $("#feedback").show();
                $("#feedback").html("¡" + this.stim.word + " no es una palabra real! Por favor pulse la tecla \'f\'");
            } else if (e.keyCode == 102 || e.keyCode == 106) {
                console.log(e.key);
                $("#feedback").show();
                $("#feedback").html("¡Correcto!");
                $("#feedback").css("color", "green");
                correctKeyPressed = true; // Set flag to true after correct key is pressed
                _s.button(e.key);
                console.log(e.keyCode);
            }
        }
    },

    button: function(response) {
        setTimeout(() => {
            _stream.apply(this);
        }, 1000);
    }
});

  //
  // slides.practicetrial = slide({
  //   name : "practicetrial",
  //   present : exp.prac_stims,
  //   start : function() {
  //   },
  //   present_handle : function(stim) {
  //     $("#feedback").hide();
  //     $("#feedback").css("color", "red");
  //
  //     console.log(stim);
  //     this.trial_start = Date.now();
  //
  //     this.stim = stim;
  //     console.log(this.stim);
  //
  //     $("#practice_word").html(this.stim.word);
  //
  //     document.onkeypress = (e) => {
  //       checkKey.call(this, e);
  //     };
  //
  //   function checkKey(e) {
  //
  //     e = e || window.event;
  //     console.log(e);
  //
  //     // 102 = f (fake word)
  //     // 106 = j (real word)
  //     if (e.keyCode != 102 && e.keyCode != 106) {
  //       $("#feedback").show();
  //       $("#feedback").html("Por favor pulse la tecla \'j\' o \'f\'");
  //     }
  //     else if (e.keyCode == 102 && this.stim.answer == 1) {
  //       $("#feedback").show();
  //       $("#feedback").html("¡" + this.stim.word + " es una palabra real! Por favor pulse la tecla \'j\'");
  //     }
  //     else if (e.keyCode == 106 && this.stim.answer == 0) {
  //       $("#feedback").show();
  //       $("#feedback").html("¡" + this.stim.word + " no es una palabra real! Por favor pulse la tecla \'f\'");
  //     }
  //     else if (e.keyCode == 102 || e.keyCode == 106) {
  //       console.log(e.key);
  //       $("#feedback").show();
  //       $("#feedback").html("¡Correcto!");
  //       $("#feedback").css("color", "green");
  //        _s.button(e.key);
  //        console.log(e.keyCode);
  //     }
  //   }
  // },
  //
  // button : function(response) {
  //   setTimeout(() => {
  //   _stream.apply(this);
  //   }, 2000);
  // }
  // });
  //

  slides.beginpage = slide({
     name : "beginpage",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.break = slide({
   name : "break",
   start: function() {
    exp.startT = Date.now();
    percentage = Math.round((exp.feedback_numcorrect / exp.feedback_numtrials) * 100);
    $("#percentage").html("Ha acertado " + percentage + "% de las veces.");
   }
 });

  slides.objecttrial = slide({
    name : "objecttrial",
    present : exp.all_stims,
    start : function() {
      // console.log(exp.all_stims);
      exp.feedback_numtrials = 0;
      exp.feedback_numcorrect = 0;
    },
    present_handle : function(stim) {
      console.log(exp.phase);

    	this.trial_start = Date.now();
      this.stim = stim;
	    console.log(this.stim);

      $("#target_word").html(this.stim.item);

      document.onkeypress = (e) => {
        checkKey.call(this, e);
      };

      function checkKey(e) {

        e = e || window.event;
        console.log(e);
        // 102 = f
        // 106 = j
        if (e.keyCode == 102 || e.keyCode == 106) {

          // update percent correct for participant feedback
          exp.feedback_numtrials += 1;
          if (this.stim.trial_type == "target" && e.keyCode ==106) {
            exp.feedback_numcorrect +=1
          }
          else if (this.stim.trial_type == "distractor" && e.keyCode == 102) {
            exp.feedback_numcorrect +=1
          }

          console.log(e.key);
          _s.button(e.key);
          console.log(e.keyCode);
        }
      }
  	},

  	button : function(response) {
      this.log_responses(response);
      if (exp.pause_trials.includes(exp.phase)){
              console.log("YESSS");
              exp.go();
              return;
      } else {
            _stream.apply(this);
      }},

    log_responses : function(response) {
          exp.data_trials.push({
            "slide_number_in_experiment" : exp.phase,
            "word": this.stim.item,
            "lemma": this.stim.lemma,
            "trial_type": this.stim.trial_type,
            "IC": this.stim.IC,
            "suffix": this.stim.suffix,
            "response": response,
            "rt" : Date.now() - _s.trial_start
          });
      }
  });


  // // Define slide
  // slides.objecttrial1 = slide({
  //     name: "objecttrial1",
  //     present: exp.all_stims,
  //     start: function () {
  //       $("#target_word").show();
  //       // if (exp.pause_trials.includes(exp.phase)){
  //       //   console.log("YESSS");
  //       //   exp.go();
  //       // }
  //     },
  //     present_handle: present_handle,
  // });

  // // Define slide
  // slides.objecttrial2 = slide({
  //     name: "objecttrial",
  //     present: exp.all_stims,
  //     start: function () {},
  //     present_handle: present_handle,
  // });


  slides.subj_info =  slide({
    name : "subj_info",
    start : function(e){
      $(".err2").hide();
    },
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        prolific_ID : $("#prolific_ID").val(),
        gender : $("#gender").val(),
        age : $("#age").val(),
        education : $("#education").val(),
        assess : $('input[name="assess"]:checked').val(),
        spanish_country_years : $("#country").val(),
        spanish_family_years : $("#family").val(),
        spanish_schhol_work_years : $("#schoolwork").val(),
        otherLanguage : $("#otherLanguage").val(),
        culture : $('#culture').val(),
        problems: $("#problems").val(),
        comments : $("#comments").val()
      };

      // The second part of the questionaire is not optional throw an
      // error if any of the questions in the second part are left unanswered
      if (exp.subj_data.prolific_ID != "" &
        exp.subj_data.gender != "" &
        exp.subj_data.age != "" &
        exp.subj_data.education != "" &
        exp.subj_data.assess != "" &
        exp.subj_data.spanish_country_years != "" &
        exp.subj_data.spanish_family_years != "" &
        exp.subj_data.spanish_schhol_work_years != "" &
        exp.subj_data.otherLanguage != "" &
        exp.subj_data.culture != "") {
        $(".err2").hide();
        exp.go(); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err2").show();
      };
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000,
      };
      fetch("https://pipe.jspsych.org/api/data/", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "*/*",
        },
        body: JSON.stringify({
          experimentID: "bILHhWBKMfwq",
          filename: "test_1.csv",
          data: exp.data,
        }),
      });
      // proliferate.submit(exp.data);
    }
  });

  return slides;
}

/*
INIT
This function creates the stimuli list

allstimuli is an array from 'stimuli.js' that contains all the stimuli in the
study. Each entry in the array is in the following format:

Return:
Dictionary with entries:
  item: name of object, single word string
  label: string of format "color_object"
*/
function init() {
  //get allStimuli
  var items_target = _.shuffle(allStimuli);
  //function that makes a dictionary of the desired output
  // format for a single stimulis
  function makeTargetStim(i) {
    //get item
    var item = items_target[i];
    var item_id = item.stimulus;
    var item_lemma = item.lemma;
    var item_IC = item.IC;
    var item_suffix = item.suffix;
    var item_trial_type = item.trial_type;

    return {
	  "item": item_id,
    "lemma": item_lemma,
    "IC": item_IC,
    "suffix": item_suffix,
    "trial_type": item_trial_type
    }
  }

  // Create empty array to store all the stims
  exp.all_stims = [];
  //for loop that iterates through all stimuli and calls on the function that
  // creates a dictionary for each stimulus
  for (var i=0; i < items_target.length; i++) {
    //call on a function that creates the stims
    // and add them to the empty array
    exp.all_stims.push(makeTargetStim(i));
  }

  // shuffle the order of items in the array to get a randomized trial order
  exp.all_stims = _.shuffle(exp.all_stims);


  exp.prac_stims = [{'word': "fumo", 'answer': "1"}, {'word': "rudaba", 'answer': "0"}, {'word': "resultó", 'answer': "1"}, {'word': "remoptaste", 'answer': "0"}]

  /*
  exp.phase number (i.e. trial number + ?) that indicates when the break should occur. his must match the total number of breaks for the script to work
  */
  exp.pause_trials = [136, 265, 394, 523] //136, 265, 394, 523

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };


  //blocks of the experiment:
  exp.structure=["consent", "i0", "practicetrial", "beginpage",
  'objecttrial', 'break',
  'objecttrial', 'break',
  'objecttrial', 'break',
  'objecttrial', 'break',
  'objecttrial', 'subj_info', 'thanks']; // "objecttrial1", "break1",

  // two variables that will track participants % of correct repsonses
  // to be displayed during each break
  exp.feedback_numtrials = 0;
  exp.feedback_numcorrect = 0;

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = 655;
  // utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#consent_button").click(function() {
      exp.go();
  });

  $("#start_practice").click(function() {
      exp.go();
  });

  $("#start_button").click(function() {
      exp.go();
  });
  $("#continue_button").click(function() {
      exp.go();
  });
  exp.go(); //show first slide
}
