
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function showSlide(id) {
  $(".slide").hide();
  $("#"+id).show();
}

function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}

function shuffle(array) {
  var currentIndex = array.length
    , temporaryValue
    , randomIndex
    ;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

function clearForm(oForm, sliderType) {
  var sliderVar = "";
  if (sliderType == 0) {
    var numSliders = NUM_STATE_SLIDERS;
    var sliderName = "stateSlider";
  } else {
    var numSliders = NUM_EMO_SLIDERS;
    var sliderName = "emoSlider";
  }
  for(var i=1; i<= numSliders; i++)
  {
    sliderVar = "#" + sliderName + i;
    $(sliderVar).slider("value", 0.5);
    $(sliderVar).css({"background":"#FFFFFF"});
    $(sliderVar + " .ui-slider-handle").css({
        "background":"#FAFAFA",
        "border-color": "#CCCCCC" });
    sliderVar = sliderName + "Value" + i;
    document.getElementById(sliderVar).style.background = "";
  }
  
  var elements = oForm.elements; 
  
  oForm.reset();

  for(var i=0; i<elements.length; i++) {
    field_type = elements[i].type.toLowerCase();
    switch(field_type) {
    
      case "text": 
      case "password": 
      case "textarea":
            case "hidden":  
        
        elements[i].value = ""; 
        break;
          
      case "radio":
      case "checkbox":
          if (elements[i].checked) {
            elements[i].checked = false; 
        }
        break;
  
      case "select-one":
      case "select-multi":
                  elements[i].selectedIndex = -1;
        break;
  
      default: 
        break;
    }
  }
}

Array.prototype.random = function() {
  return this[random(this.length)];
}

function setQuestion(array) {
    var i = random(0, array.length - 1);
    var q = array[i];
    return q;
}

function shuffledArray(arrLength)
{
  var j, tmp;
  var arr = new Array(arrLength);
  for (i = 0; i < arrLength; i++)
  {
    arr[i] = i;
  }
  for (i = 0; i < arrLength-1; i++)
  {
    j = Math.floor((Math.random() * (arrLength - 1 - i)) + 0.99) + i;
    tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
  return arr;
}

function shuffledSampleArray(arrLength, sampleLength)
{
  var arr = shuffledArray(arrLength);
  var beginIndex = Math.floor(Math.random() * (arrLength-sampleLength+1));
  return arr.slice(beginIndex, beginIndex+sampleLength);
}

function getRadioCheckedValue(formNum, radio_name)
{
   var oRadio = document.forms[formNum].elements[radio_name];
   for(var i = 0; i < oRadio.length; i++)
   {
      if(oRadio[i].checked)
      {
         return oRadio[i].value;
      }
   }
   return '';
}

function randomizeSharpOffset()
{
  
  var r = Math.floor((Math.random()*6)+1);
  if (r < 4) { return r; }
  else { return 3-r; }
  /*
  var r = Math.floor((Math.random()*3)+1);
  return r;
  */
}

//var quantifiersArray = ["some", "all"];

var allConditions = 
[
[
{"imageID": 1, "image": "weather-amazing1.jpg", "category": "amazing"},
{"imageID": 2, "image": "weather-amazing2.jpg", "category": "amazing"},
{"imageID": 3, "image": "weather-amazing3.jpg", "category": "amazing"},
{"imageID": 4, "image": "weather-ok1.jpg", "category": "ok"},
{"imageID": 5, "image": "weather-ok2.jpg", "category": "ok"},
{"imageID": 6, "image": "weather-ok3.jpg", "category": "ok"},
{"imageID": 7, "image": "weather-terrible1.jpg", "category": "terrible"},
{"imageID": 8, "image": "weather-terrible2.jpg", "category": "terrible"},
{"imageID": 9, "image": "weather-terrible3.jpg", "category": "terrible"},
]
];

var allAffects = ["sad", "disgusted", "angry", "neutral", "content", "happy", "excited"];
var shuffledAffects = shuffle(allAffects);

var allStates = ["terrible", "not bad", "amazing"];
var shuffledStates = shuffle(allStates);

var girlNames = ["Ann", "Barbara", "Cathy", "Diana", "Emma", "Fiona", "Grace", "Heather", "Iris", "Jane", "Kathy",
"Lena", "Mary", "Nancy", "Olga", "Patty", "Rebecca", "Stephanie", "Tracy", "Victoria", "Wendy", "Yvonne",];

var boyNames = ["Albert", "Bob", "Calvin", "David", "Edward", "Frank", "George", "Henry", "Ivan", "Jake", "Kevin", "Larry",
"Matt", "Nathan", "Oliver", "Patrick", "Robert", "Steven", "Tom", "Victor", "Winston", "Zack"];


var allNames = ["Ann", "Barbara", "Cathy", "Diana", "Emma", "Fiona", "Grace", "Heather", "Iris", "Jane", "Kathy",
"Lena", "Mary", "Nancy", "Olga", "Patty", "Rebecca", "Stephanie", "Tracy", "Victoria", "Wendy", "Yvonne", 
"Albert", "Bob", "Calvin", "David", "Edward", "Frank", "George", "Henry", "Ivan", "Jake", "Kevin", "Larry",
"Matt", "Nathan", "Oliver", "Patrick", "Robert", "Steven", "Tom", "Victor", "Winston", "Zack"];
allNames = shuffle(allNames);

var debug = false;
if(debug) { allConditions = debugConditions; }

var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var shuffledTrials = shuffle(allConditions[chooseCondition]);
var numTrials = shuffledTrials.length;
//var numTrials = 2;
//var shuffledOrder = shuffledSampleArray(allTrialOrders.length, numTrials);
var currentTrialNum = 0;
var trial;
var numComplete = 0;

var images = new Array();

function preload() {
        for (i = 0; i < numTrials; i++) {
          images[i] = new Image()
          images[i].src = "https://web.stanford.edu/~justinek/irony_exp/images/".concat(shuffledTrials[i].image);
        }
      }

preload();

showSlide("instructions");
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);


var experiment = {
  condition: chooseCondition + 1,
  imageIDs: new Array(numTrials),
  imageCategories: new Array(numTrials),

  stateRatings: new Array(numTrials),

  probsSad: new Array(numTrials),
  probsDisgusted: new Array(numTrials),
  probsAngry: new Array(numTrials),
  probsNeutral: new Array(numTrials),
  probsContent: new Array(numTrials),
  probsHappy: new Array(numTrials),
  probsExcited: new Array(numTrials),

  orders: new Array(numTrials),

  gender: "",
  age:"",
  income:"",
  nativeLanguage:"",
  comments:"",

  description: function() {
    showSlide("description");
    $("#tot-num").html(numTrials);  
  },
  end: function() {
    var gen = getRadioCheckedValue(2, "genderButton");
    var ag = document.age.ageRange.value;
    var lan = document.language.nativeLanguage.value;
    var comm = document.comments.input.value;
    var incomeVal = document.income.incomeRange.value;
    experiment.gender = gen;
    experiment.age = ag;
    experiment.nativeLanguage = lan;
    experiment.comments = comm;
    experiment.income = incomeVal;
    //clearForm(document.forms[1]);
    clearForm(document.forms[2]);
    clearForm(document.forms[3]);
    clearForm(document.forms[4]);
    clearForm(document.forms[5]);
    //clearForm(document.forms[6]);    
    showSlide("finished");
    setTimeout(function() {turk.submit(experiment) }, 1500);
  },
  next: function() {
    if (numComplete > 0) {

      experiment.stateRatings[currentTrialNum] = getRadioCheckedValue(0, "weather");
      
      experiment.probsSad[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("sad") + 1)).value);
      experiment.probsDisgusted[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("disgusted") + 1)).value);
      experiment.probsAngry[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("angry") + 1)).value);
      experiment.probsNeutral[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("neutral") + 1)).value);
      experiment.probsContent[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("content") + 1)).value);
      experiment.probsHappy[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("happy") + 1)).value);
      experiment.probsExcited[currentTrialNum] = parseFloat(document.getElementById("emoSliderValue".concat(shuffledAffects.indexOf("excited") + 1)).value);
      
      
      experiment.orders[currentTrialNum] = numComplete;
      //experiment.preciseEatenQuants[currentTrialNum] = document.getElementById("preciseEatenQuant").innerHTML;
          
      clearForm(document.forms[0]);
      clearForm(document.forms[1]);
    }
    if (numComplete >= numTrials) {
      $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
      $("#trial-num").html(numComplete);
      $("#total-num").html(numTrials);
      showSlide("askInfo");
    } else {
      $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
      $("#trial-num").html(numComplete);
      $("#total-num").html(numTrials);
      currentTrialNum = numComplete;

      trial = shuffledTrials[numComplete];
      experiment.imageIDs[currentTrialNum] = trial.imageID;
      experiment.imageCategories[currentTrialNum] = trial.category;

      showSlide("stage");
      name = allNames.shift();
      pronoun = "";
      if (girlNames.indexOf(name) > -1) {
        pronoun = "she";
      } else {
        pronoun = "he";
      }      
      $("#utterance").html(trial.sentence);
      $("#name1").html(name);
      $("#name2").html(name);
      $("#name3").html(name);
      var imageName = "https://web.stanford.edu/~justinek/irony_exp/images/".concat(trial.image);
      var input = document.getElementById('weather-image')
      input.src = imageName;
      //document.getElementById('weather-image') = images[currentTrialNum];
      $("#pronoun1").html(pronoun);

      $("#stateSlider1 .ui-slider-handle").hide();
      $("#stateSlider2 .ui-slider-handle").hide();
      $("#stateSlider3 .ui-slider-handle").hide();
      $("#emoSlider1 .ui-slider-handle").hide();
      $("#emoSlider2 .ui-slider-handle").hide();
      $("#emoSlider3 .ui-slider-handle").hide();
      $("#emoSlider4 .ui-slider-handle").hide();
      $("#emoSlider5 .ui-slider-handle").hide();
      $("#emoSlider6 .ui-slider-handle").hide();
      $("#emoSlider7 .ui-slider-handle").hide();

      for (var i=1; i <= NUM_STATE_SLIDERS; i++) {
        $("#state".concat(i)).html(shuffledStates[i-1]);
      }
      

      for (var i=1; i <= NUM_EMO_SLIDERS; i++) {
        $("#affect".concat(i)).html(shuffledAffects[i-1]);
      }


      numComplete++;
    }
  }
}

var NUM_STATE_SLIDERS = 3;
var NUM_EMO_SLIDERS = 7;


$("#stateSlider1").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("stateSlider1 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#stateSlider1 .ui-slider-handle').show();
                   $("#stateSlider1 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#stateSliderValue1').attr('value', ui.value);
                   $("#stateSlider1").css({"background":"#FFFFFF"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#stateSlider1 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#stateSlider2").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("stateSlider2 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#stateSlider2 .ui-slider-handle').show();
                   $("#stateSlider2 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#stateSliderValue2').attr('value', ui.value);
                   $("#stateSlider2").css({"background":"#FFFFFF"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#stateSlider2 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#stateSlider3").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("stateSlider3 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#stateSlider3 .ui-slider-handle').show();
                   $("#stateSlider3 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#stateSliderValue3').attr('value', ui.value);
                   $("#stateSlider3").css({"background":"#FFFFFF"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#stateSlider3 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});


$("#emoSlider1").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider1 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider1 .ui-slider-handle').show();
                   $("#emoSlider1 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue1').attr('value', ui.value);
                   $("#emoSlider1").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider1 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider2").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider2 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider2 .ui-slider-handle').show();
                   $("#emoSlider2 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue2').attr('value', ui.value);
                   $("#emoSlider2").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider2 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider3").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider3 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider3 .ui-slider-handle').show();
                   $("#emoSlider3 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue3').attr('value', ui.value);
                   $("#emoSlider3").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider3 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider4").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider4 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider4 .ui-slider-handle').show();
                   $("#emoSlider4 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue4').attr('value', ui.value);
                   $("#emoSlider4").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider4 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider5").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider5 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider5 .ui-slider-handle').show();
                   $("#emoSlider5 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue5').attr('value', ui.value);
                   $("#emoSlider5").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider5 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider6").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider6 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider6 .ui-slider-handle').show();
                   $("#emoSlider6 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue6').attr('value', ui.value);
                   $("#emoSlider6").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider6 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#emoSlider7").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $("emoSlider7 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#emoSlider7 .ui-slider-handle').show();
                   $("#emoSlider7 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#emoSliderValue7').attr('value', ui.value);
                   $("#emoSlider7").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#emoSlider7 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});



