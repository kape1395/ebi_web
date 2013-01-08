
function ebi_init() {
    ebi_models_init();
    $("#main-tabs > ul > li:first > a").tab('show');
}


// =============================================================================
//  Models tab
// =============================================================================

function ebi_models_init() {
    $("a[href='#model-list']").click(ebi_model_list_show);
    $("a[href='#model-new']").click(ebi_model_new_show);
    $("#model-list").on("click", "a[href='#model-view']", function () {ebi_model_show($(this).data("modelref"));});
  //$("#model-list").on("click", "a[href='#object-view']", function () {ebi_model_object_show($(this).data("objectref"));});
    $("#model-list").on("click", "a[href='#object-view']", function () {});
    $("#models > div").hide();
    $("#model-list").show();
    ebi_model_list_load();
    ebi_model_list_show();
}

function ebi_models_show() {
    $("#main-tabs ul li a[href = '#models']").tab('show');
}

// -----------------------------------------------------------------------------
//  Model: list
//

function ebi_model_list_show() {
    $("#models > div").hide();
    $("#model-list").show();
    ebi_models_show();
}

function ebi_model_list_load() {
    var o1 = {"ref": "o1r", "id": "o1", "desc": "CNT Perf"};
    var o2 = {"ref": "o2r", "id": "o2", "desc": "Simple"};

    var m1 = {"ref": "m1r", "id": "0.0.0.31", "name": "model 1", "description": "", "object": o1};
    var m2 = {"ref": "m2r", "id": "0.0.0.32", "name": "model 2", "description": "desc", "object": o2};
    var m3 = {"ref": "m3r", "id": "0.0.0.33", "name": "model 3", "description": "Fout layer two-dimensional model with finite k_3", "object": null};
    ebi_model_list_render([m1, m2, m3]);
}

function ebi_model_list_render(models) {
    var rows = "";
    for (var i = 0; i < models.length; i++) {
        var m = models[i];
        rows += "<tr>";
        if (m.object == null) {
            rows += "<td>&nbsp;</td>";
        } else {
            rows += "<td><a href='#object-view' data-objectref='" + m.object.ref + "'>" + m.object.desc + "</td>";
        }
        rows += "<td><a href='#model-view' data-modelref='" + m.ref + "'>" + m.name + " <span class='id-text'>(" + m.id + ")</span></a></td>";
        rows += "<td>" + m.description + "</td>";
        rows += "</tr>";
    }
    $("#model-list-table > tbody").html(rows);
}


// -----------------------------------------------------------------------------
//  Model: new
//

function ebi_model_new_show() {
    $("#models > div").hide();
    $("#model-new").show();
    ebi_models_show();
}

function ebi_model_show(modelRef) {
    console.log("ebi_model_show: " + modelRef);
}