
let inputs = [
    {key:"mmr_filename",control:'input',type:"text",default:"file1.csv",value:"",col:0,units:""},
    {key:"smr_filename",control:'input',type:"text",default:"file2.csv",value:"",col:0,units:""},
    
    {key:"animal1",control:'input',type:"text",default:"fish",value:"",col:1,units:""},
    {key:"animal2",control:'input',type:"text",default:"cow",value:"",col:1,units:""},
    {key:"animal3",control:'input',type:"text",default:"bird",value:"",col:1,units:""},
    {key:"animal4",control:'input',type:"text",default:"monkey",value:"",col:1,units:""},
    {key:"weight1",control:'input',type:"number",default:0.2,value:"",col:2,units:"kg"},
    {key:"weight2",control:'input',type:"number",default:0.2,value:"",col:2,units:"kg"},
    {key:"weight3",control:'input',type:"number",default:0.2,value:"",col:2,units:"kg"},
    {key:"weight4",control:'input',type:"number",default:0.2,value:"",col:2,units:"kg"},
    {key:"respV1",control:'input',type:"number",default:1,value:"",col:3,units:"ml/min"},
    {key:"respV2",control:'input',type:"number",default:2,value:"",col:3,units:"ml/min"},
    {key:"respV3",control:'input',type:"number",default:3,value:"",col:3,units:"ml/min"},
    {key:"respV4",control:'input',type:"number",default:4,value:"",col:3,units:"ml/min"},

    {key:"r2_threshold_smr",control:'input',type:"number",default:0.85,value:"",col:0,units:" unitless, <1"},
    {key:"r2_threshold_mmr",control:'input',type:"number",default:0.9,value:"",col:0,units:"unitless, <1"},
    {key:"min_length_mmr",control:'select',type:"",default:"60",value:"",col:0,units:"minutes", options:[
        {value:"60",display:"60 second steepest MO2 slope"},
        {value:"90",display:"90 second steepest MO2 slope"},
        {value:"120",display:"2 min steepest MO2 slope"},
    ]},
    {key:"background_prior",control:'select',type:"",default:"NA",value:"",col:0,units:"-",options:[
        {value:"NA",display:"NA"},
        {value:"SOMETHING ELSE",display:"SOMETHING ELSE"},
    ]},
    {key:"background_post",control:'select',type:"",default:"NA",value:"",col:0,units:"-",options:[
        {value:"NA",display:"NA"},
        {value:"SOMETHING ELSE",display:"SOMETHING ELSE"},
    ]},
    {key:"match_background_Ch",control:'select',type:"",default:"FALSE",value:"",col:0,units:"-",options:[
        {value:"TRUE",display:"TRUE, yes match background"},
        {value:"FALSE",display:"FALSE, no, do not match background"},
    ]},
    
    
];


function validate(){

    //perform a series of checks here to make sure we are OK
    let errors =[];


    //check if all weights are <= 100
    inputs.filter(e => e.units == "kg").forEach(function(e){
        if(e.value>100){
            errors.push({
                msg:e.key+" of "+e.value+" is larger than 100 kg"
                
            });
        }
    });


    ////



    //const found = array1.find(element => element > 10);
    return errors;
}


String.prototype.format = function() {
    var formatted = this;
    for (var i = 0; i < arguments[0].length; i++) {
        var regexp = new RegExp('\\{'+arguments[0][i].key+'\\}', 'gi');
        formatted = formatted.replace(regexp, arguments[0][i].val);
    }
    return formatted;
};


//var s = 'The {0} is dead. Don\'t code {0}. Code {1} that is open source!'.format('ASP', 'PHP');

function printOutput(){
    $('#errors').html('');
    var errors = validate();

    if(errors.length<1){

        var string ='MMR_SMR_AS_EPOC(<br>\
            data.MMR = "{mmr_filename}",<br>\
            data.SMR = "{smr_filename}",<br>\
            AnimalID = c("{animal1}","{animal2}","{animal3}","{animal4}"),<br>\
            BW.animal = c({weight1},{weight2},{weight3},{weight4}),<br>\
            resp.V = c({respV1},{respV2},{respV3},{respV4}),<br>\
            r2_threshold_smr = {r2_threshold_smr},<br>\
            r2_threshold_mmr = {r2_threshold_mmr},<br>\
            min_length_mmr = {min_length_mmr},<br>\
            background_prior = {background_prior},<br>\
            background_post = {background_post},<br>\
            match_background_Ch = {match_background_Ch})'.format(inputs.map(function(u){
                return {key:u.key,val:u.value?u.value:u.default}
            }));/*
            [mmr_filename, smr_filename,
            animal1, animal2, animal3, animal4,
            weight1, weight2, weight3, weight4,
            respV1, respV2, respV3, respV4]
        );*/

        $('#output').html(string);

    }else{
        for(y=0;y<errors.length;y++){
            let e = errors[y];
            $('#errors').append($('<div>',{html:e.msg}));
        }
    }
}

function addInputs(){
    let cols = {};
    for(t=0;t<inputs.length;t++){//.forEach(function(i){
        let i = inputs[t];

        if(!cols[i.col]){
            cols[i.col]=[];
        }
        cols[i.col].push(i);
    }

    let colKeys = Object.keys(cols);
    let table = $('<table>',{});
    let trow = $('<tr>',{});
    for(r = 0;r<colKeys.length;r++){//.function
        
        let tcol = $('<td>',{valign:'top'});

        for(var u =0;u<cols[colKeys[r]].length;u++){

            let i = cols[colKeys[r]][u];
            var ip = $('<'+i.control+'>',{type:i.type,placeholder:i.default,value:i.value,id:i.key});
            if(i.control=="select"){
                i.options.forEach(function(op){
                    ip.append($('<option>',{value:op.value,html:op.display,selected:op.value==i.default?true:false}));
                });
                ip.change(function(){
                    for(var x = 0;x<inputs.length;x++){
                        if(inputs[x].key==i.key){
                            inputs[x].value=this.value;
                        }
                    }
                    printOutput();
                });
            }else{
                ip.keyup(function(){
                    for(var x = 0;x<inputs.length;x++){
                        if(inputs[x].key==i.key){
                            inputs[x].value=this.value;
                        }
                    }
                    printOutput();
                });
            }
            
            var ipl = $('<label>',{for:i.key,html:i.key});
            ipl.append(ip);
            ipl.append(i.units);
            var ipc = $('<div>',{}).append(ipl);

            tcol.append(ipc);
        }
        trow.append(tcol);
    }
    table.append(trow);
    $('#inputs').append(table);
}

addInputs();
