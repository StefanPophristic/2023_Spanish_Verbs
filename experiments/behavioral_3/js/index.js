//https://www.ncbi.nlm.nih.gov/pmc/articles/doi/10.3389/fpsyg.2014.00399/full// Returns a random integer between min (included) and max (excluded)


/*
Global variables

fixactionCrossTime: how much time (ms) should the fixation cross be presented
crossStimTime: how much time (ms) should pass between the fixation cross and the
  onset of the visually presented stimulus
stimCrossTime: how much time (ms) should pass between the stimulus and the next trial

If you want any of these times to differ for each trial, turn this into a function
and then call on the function in the script below
*/

fixationCrossTime = 300
crossStimTime = 300
stimCrossTime = 0


function ISI() {
  return (fixationCrossTime + crossStimTime)
}
// Add studyDuration time to all texts that require it.
$(document).ready(function() {
  // Update all elements with the class "studyDuration"
  $(".studyDuration").html(studyDuration);
});

// randomly assign ISI as a value between 600-800 mix
// function ISI(min = 600, max = 800) {
//   return Math.floor(Math.random() * (max - min + 1)) + min;
// }

// Parameter to automatically change number of minutes in study instructions
var studyDuration =  "45"

/*
Input: object containing a .structure element, which is an array with the slide structures
Output: none

make_slides defines each type of slide. The function then creates each slide as defined
by the input f.structure.

Each slide must contain the following field:
  name: (string) name of the slide as referenced by f.structure

The slide may also contain the following information:
  start: (function) everything that must be exectued at the start of slide. This
    includes defining variables such as the start time of the block, and hiding all
    html elements from previous slides. Note that this does not change elements per trial,
    but rather per block.
  present: exp.prac_stims, exp.all_stims --> lets the stims be visiuble for present_handle
  present_handle: (function) defines what happens on the slide. Include everything
    that should appear on the slide, and code in dynamic elements (i.e. button presses)

The rest of the fields defined in each slide are functions that the above fields reference.
These may include the following:
  button: defines what the script should do once the "button" (i.e. an admissable
    response, whether that is a button press or input text, as defined in
    present_handle) is pressed. I.e. it says, save the responses, and either continue
    itterating through the stims, or if they are exhausted, go onto the next slide type
  log_responses: defines how to save participant response
  submit: defines how to save information from the post-experiment survey.

You can add your own functions as necessary.
*/

function make_slides(f) {
  var slides = {};

  // Consent slide: Show the consent slide (text defined in index.html)
  //  and save experiment start time.
  slides.consent = slide({
     name : "consent",
     start: function() {
      exp.startT = Date.now();
     }
  });

  // instructions slide: show the instructions (text defined in index.html)
  slides.instructions_1 = slide({
     name : "instructions_1",
     start: function() {
     }
  });

  // practice slides:
  slides.practicetrial = slide({
    name: "practicetrial",
    present: exp.prac_stims,
    present_handle: function(stim) {
        // hide feedback text
        $("#feedback").hide();
        $("#feedback").css("color", "rgb(255, 150, 159)");

        console.log(stim);
        this.trial_start = Date.now();

        this.stim = stim;
        console.log(this.stim);

        // show the stim
        $("#practice_word").html(this.stim.word);

        // Flag to track if correct key has been pressed
        let correctKeyPressed = false;

        // define what to do when any key is pressed
        document.onkeypress = (e) => {
            if (!correctKeyPressed) {
              // Check if correct key has not been pressed yet
              checkKey.call(this, e);
            }
        };

        // function that checks whether a valid key has been pressed
        // 102 = f (fake word)
        // 106 = j (real word)
        function checkKey(e) {
            e = e || window.event;
            console.log(e);

            // Show feedback if no correct key was pressed
            if (e.keyCode != 102 && e.keyCode != 106) {
                $("#feedback").show();
                $("#feedback").html("Por favor pulse la tecla \'j\' o \'f\'");
            } // Show feedback if incorrect key was pressed
            else if (e.keyCode == 102 && this.stim.answer == 1) {
                $("#feedback").show();
                $("#feedback").html("¡" + this.stim.word + " es una palabra real! Por favor pulse la tecla \'j\'");
            } // Show feedback if incorrect key was pressed
            else if (e.keyCode == 106 && this.stim.answer == 0) {
                $("#feedback").show();
                $("#feedback").html("¡" + this.stim.word + " no es una palabra real! Por favor pulse la tecla \'f\'");
            } // Show feedback if correct key was pressed
            else if (e.keyCode == 102 || e.keyCode == 106) {
                console.log(e.key);
                $("#feedback").show();
                $("#feedback").html("¡Correcto!");
                $("#feedback").css("color", "rgb(225, 255, 207)");
                correctKeyPressed = true; // Set flag to true after correct key is pressed
                _s.button(e.key);
                console.log(e.keyCode);
            }
        }
    },

    // After each practice stim, move onto the next one with a 1000 ms delay
    button: function(response) {
        setTimeout(() => {
            _stream.apply(this);
        }, 1000);
    }
  });

  // Begining Page with more instructions (text defined in index.html)
  slides.beginpage = slide({
     name : "beginpage",
     start: function() {
      exp.startT = Date.now();
     }
  });

  // Break page
  slides.break = slide({
   name : "break",
   start: function() {
    exp.startT = Date.now();

    // calculate percentage correct in this round
    percentage = Math.round((exp.feedback_numcorrect / exp.feedback_numtrials) * 100);
    $("#percentage").html("Ha acertado " + percentage + "% de las veces.");
   }
  });

  // objectrial: Main slides for experiment
  slides.objecttrial = slide({
    name : "objecttrial",
    present : exp.all_stims,
    start : function() {
      // Set the counters for total number of trials and number of correct trails to
      // 0, this will be reset for each block
      exp.feedback_numtrials = 0;
      exp.feedback_numcorrect = 0;
    },
    present_handle : function(stim) {

      // Remove any existing keypress handlers at start of each trial to avoid multiple handlers
      document.onkeypress = null;

      // present fixation cross
      $("#target_word").hide();
      $("#fixation_cross").show();

      // set the new stim
      console.log(exp.phase);
    	this.trial_start = Date.now();
      this.stim = stim;
	    console.log(this.stim);
      $("#target_word").html(this.stim.item);

      // After set amount of time, hide fixation cross
      setTimeout(() => {
        $("#fixation_cross").hide();
      }, fixationCrossTime);

      // // after set amount of time, show the stim
      // setTimeout(() => {
      //   $("#target_word").show();
      // }, fixationCrossTime + crossStimTime);

      this.trial_ISI = ISI();

      console.log(this.trial_ISI)

      // after set amount of time, show the stim
      setTimeout(() => {
        $("#target_word").show();
        // Tracker to see if a keypress has been processed (to prevent multiple button presses stacking up)
        this.processingKey = false;
      }, this.trial_ISI);

      // Tracker to see if a keypress has been processed (to prevent multiple button presses stacking up)
      this.processingKey = true;

      document.onkeypress = (e) => {
        checkKey.call(this, e);
      };

      /*
      Takes in keypress event 'e' and cehcks whether it is one of the
      key presses allowed in the experiment. If so, pass it to the button function,
      if not, return to above function and wait for next keypress.

      Key codes:
      102 = f
      106 = j
      */
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

      // Prevent processing if already in progress (prevents participants from just clickign the j button like crazy and stacking responses)
      if (this.processingKey) return;

      // Set the processing key to true in order to prevent further processing
      this.processingKey = true;

      // call on log responses function to save the response
      this.log_responses(response);

      if (exp.pause_trials.includes(exp.phase)) {

        // hide target word
        $("#target_word").hide();

        // reseet flag after trial finishes
        this.processingKey = false;

        setTimeout(() => {
          exp.go();
        }, stimCrossTime);
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
      proliferate.submit(exp.data);
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
  exp.pause_trials = [200, 400, 600, 800, 1000, 1200] //136, 265, 394, 523

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




  // Create loop for creation of breaks



  //blocks of the experiment:
  exp.structure=["consent", "instructions_1", "practicetrial", "beginpage",
  'objecttrial', 'break',
  'objecttrial', 'break',
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

  exp.nQs = 1404;
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
