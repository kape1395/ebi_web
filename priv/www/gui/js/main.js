
function ebi_init() {
    ebi_models_init();
    ebi_moded_init();
    ebi_pages_show_main_overview();
}

function api_url(resource) {
    return "/ebi/api/" + resource;
}


//=============================================================================
//  Master pages
//=============================================================================

function ebi_pages_show_main() {
    $("#pages > div").hide();
    $("#main-tabs").show();
}

function ebi_pages_show_main_overview() {
    ebi_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#overview']").tab('show');
}

function ebi_pages_show_moded() {
    $("#pages > div").hide();
    $("#moded").show();
}



// =============================================================================
//  Main tabs: models tab
// =============================================================================

function ebi_models_init() {
    $("#main-tabs").on("click", "a[href='#model-list']", function () {
        ebi_model_list_load();
        ebi_model_list_show();
    });
    $("a[href='#model-new']").click(ebi_moded_new);
    $("#model-list").on("click", "a[href='#model-view']", function () {
        var modelId = $(this).data("modelid");
        var modelRef = $(this).data("modelref");
        ebi_model_show(modelId, modelRef);
    });
  //$("#model-list").on("click", "a[href='#object-view']", function () {ebi_model_object_show($(this).data("objectref"));});
    $("#model-list").on("click", "a[href='#object-view']", function () {});
    $("#models > div").hide();
    $("#model-list").show();

    ebi_model_list_load();
}

function ebi_models_show() {
    $("#main-tabs ul li a[href = '#models']").tab('show');
}

// -----------------------------------------------------------------------------
//  Model: list
//

function ebi_model_list_load() {
    $.getJSON(api_url("model"), function (data, textStatus, jqXHR) {
        ebi_model_list_render(data);
    });
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
        rows += "<td><a href='#model-view' data-modelid='" + m.id + "' data-modelref='" + m.ref + "'>" + m.name + " <span class='id-text'>(" + m.id + ")</span></a></td>";
        rows += "<td>" + m.description + "</td>";
        rows += "</tr>";
    }
    $("#model-list-table > tbody").html(rows);
}

function ebi_model_list_show() {
    $("#models > div").hide();
    $("#model-list").show();
    ebi_models_show();
}


// -----------------------------------------------------------------------------
//  Model Details
//

function ebi_model_new_show() {
    $("#models > div").hide();
    $("#model-new").show();
    ebi_models_show();
}

function ebi_model_show(modelId, modelRef) {
    $("#models > div").hide();
    $("#model-details").show();
    $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
        ebi_model_render(data);
    });
}

function ebi_model_render(model) {
    $("#model-name").html("");
    $("#model-description").html("");
    $("#model-reactions > tbody").html("");
    $("#model-species").html("");
    if (model == null) {
        return;
    }
    $("#model-name").html("" + model.name + " (" + model.id +")");
    $("#model-description").html(model.description);

    if (model.definition == null) {
        return;
    }
    ebi_model_render_reactions(model);
    ebi_model_render_species(model);
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}

function ebi_model_render_reactions(model) {
    var reactions = model.definition.reactions;
    var str = "";
    for (var i = 0; i < reactions.length; i++) {
        var r = reactions[i];
        if (r.type == "ebi_rdef_mm") {
            str += "<tr>";
            str += "<td><math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
            str += "    <mrow>";
            str += "        <mi>E</mi><mo>+</mo><mi>" + r.definition.substrate + "</mi>";
            str += "        <mo>&harr;</mo>";
            str += "        <mi mathvariant='italic'>E" + r.definition.substrate + "</mi>";
            str += "        <mo>&rarr;</mo>";
            str += "        <mi>E</mi><mo>+</mo><mi mathvariant='italic'>" + r.definition.product + "</mi>";
            str += "    </mrow>";
            str += "</math>,<td>";
            str += "<td><math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
            str += "    <mrow>";
            str += "        <mrow><mo>" + r.name + "</mo></mrow>";
            str += "        <mo>=</mo>";
            str += "        <mfrac>";
            str += "            <mrow><mi mathvariant='italic'>" + r.definition.vmax + "</mi><mi mathvariant='italic'>" + r.definition.substrate + "</mi></mrow>";
            str += "            <mrow><mi mathvariant='italic'>" + r.definition.km + "</mi><mo>+</mo><mi mathvariant='italic'>" + r.definition.substrate + "</mi></mrow>";
            str += "        </mfrac>";
            str += "    </mrow>";
            str += "</math>,</td>";
            str += "<td><span class='pull-right'>(" + (i + 1) + ")</span></td>";
            str += "</tr>";
        } else if (r.type == "ebi_rdef_simple") {
            str += "<tr>";
            str += "<td><math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
            str += "    <mrow>";
            for (var j = 0; j < r.definition.reagents.length; j++) {
                var rr = r.definition.reagents[j];
                if (j > 0) str += "<mo>+</mo>";
                if (rr.number != 1) str += "<mn>" + rr.number + "</mn>";
                str += "<mi mathvariant='italic'>" + rr.species + "</mi>";
            }
            str += "<mo>&rarr;</mo>";
            for (var j = 0; j < r.definition.products.length; j++) {
                var rr = r.definition.products[j];
                if (j > 0) str += "<mo>+</mo>";
                if (rr.number != 1) str += "<mn>" + rr.number + "</mn>";
                str += "<mi mathvariant='italic'>" + rr.species + "</mi>";
            }
            str += "    </mrow>";
            str += "</math>,<td>";
            str += "<td><math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
            str += "    <mrow>";
            str += "        <mrow><mo>" + r.name + "</mo></mrow>";
            str += "        <mo>=</mo>";
            str += "        <mrow>";
            str += "<mi>" + r.definition.rateconst + "</mi>";
            for (var j = 0; j < r.definition.reagents.length; j++) {
                str += "<mi mathvariant='italic'>" + r.definition.reagents[j].species + "</mi>";
            }
            str += "        </mrow>";
            str += "    </mrow>";
            str += "</math>,</td>";
            str += "<td><span class='pull-right'>(" + (i + 1) + ")</span></td>";
            str += "</tr>";
        } else {
            str += "<tr><td>Unknown: " + r.type + "</td><td>" + reactions[i].name + "</td>";
            str += "<td>Unsupported reaction type</td><td>&nbsp;</td>";
            str += "<td><span class='pull-right'>(" + (i + 1) + ")</span></td>";
            str += "</tr>";
        }
    }
    $("#model-reactions > tbody").html(str);
}

function ebi_model_render_species(model) {
    var str = "";
    for (var i = 0; i < model.definition.species.length; i++) {
        var s = model.definition.species[i];
        str += "<li>" + s.name + " - " + s.description + ".</li>";
    }
    $("#model-species").html(str);
}


// =============================================================================
//  Master pages: Model Editor
// =============================================================================

function ebi_moded_new() {
    var model = {
        id: null,
        ref: null,
        name: "",
        description: "",
        status: "active",
        definition: {
            species: [],
            reactions: [],
            compartments: []
        },
        parameters: []
    };
    ebi_moded_model_set(model);
    ebi_moded_render();
    ebi_moded_show_general();
}

function ebi_moded_edit(modelId) {
    $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
        ebi_moded_model_set(data);
        ebi_moded_render();
        ebi_moded_show_general();
    });
}

function ebi_moded_copy(modelId) {
    $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
        data.id = null;
        data.ref = null;
        data.name = "Copy of " + data.name;
        ebi_moded_model_set(data);
        ebi_moded_render();
        ebi_moded_show_general();
    });
}

function ebi_moded_show_step(name) {
    ebi_pages_show_moded();
    $("#moded > div.tab-content > div").hide();
    $("#moded > div.tab-content > div#" + name).show();
    $("#moded > ul.breadcrumb a").removeClass("current-moded-step");
    $("#moded > ul.breadcrumb a[href='#" + name + "']").addClass("current-moded-step");
}

function ebi_moded_show_general() {
    ebi_pages_show_moded();
    ebi_moded_show_step("moded-general");
}

function ebi_moded_model() {
    return $("#moded").data("model");
}

function ebi_moded_model_set(model) {
    $("#moded").data("model", model);
}

function ebi_moded_init() {
    $("#moded").on("click", "a[href='#moded-general']",     ebi_moded_show_general);
    $("#moded").on("click", "a[href='#moded-species']",     function () {ebi_moded_show_step("moded-species");});
    $("#moded").on("click", "a[href='#moded-reactions']",   function () {ebi_moded_show_step("moded-reactions");});
    $("#moded").on("click", "a[href='#moded-domain']",      function () {ebi_moded_show_step("moded-domain");});
    $("#moded").on("click", "a[href='#moded-parameters']",  function () {ebi_moded_show_step("moded-parameters");});
    $("#moded").on("click", "a[href='#moded-done']",        function () {ebi_moded_show_step("moded-done");});

    //
    //  General
    //
    $("#moded-gen-name").change(function () {
        ebi_moded_model().name = $(this).val();
    });
    $("#moded-gen-desc").change(function () {
        ebi_moded_model().description = $(this).val();
    });

    //
    //  Species
    //
    $("#moded").on("change", ".model-spc-name", function () {
        ebi_moded_model().definition.species[$(this).closest("tr").data("idx")].name = $(this).val();
        ebi_moded_render_species();
    });
    $("#moded").on("change", ".model-spc-desc", function () {
        ebi_moded_model().definition.species[$(this).closest("tr").data("idx")].description = $(this).val();
        ebi_moded_render_species();
    });
    $("#moded").on("click", "a[href='#moded-species-add']", function () {
        ebi_moded_model().definition.species.push({name:"", description:""});
        ebi_moded_render_species();
    });
    $("#moded").on("click", "a[href='#moded-species-rem']", function () {
        ebi_moded_model().definition.species.splice($(this).closest("tr").data("idx"), 1);
        ebi_moded_render_species();
    });

    //
    //  Reactions
    //
}

function ebi_moded_render() {
    var model = ebi_moded_model();
    $("#moded-gen-name").val(model.name);
    $("#moded-gen-desc").val(model.description);
    ebi_moded_render_species();
}

function ebi_moded_render_species() {
    var model = ebi_moded_model();
    var str = "";
    for (var i = 0; i < model.definition.species.length; i++) {
        var s = model.definition.species[i];
        str += "<tr data-idx='" + i + "'>";
        str += "<td><input type='text' placeholder='Species name' value='" + s.name + "' class='model-spc-name'/></td>";
        str += "<td><input type='text' placeholder='Description' value='" + s.description + "' class='model-spc-desc input-xlarge'/></td>";
        str += "<td><a href='#moded-species-rem' class='btn pull-right'>Remove</a></td>";
        str += "</tr>";
    }
    str += "<tr><td colspan='3'><a href='#moded-species-add' class='btn btn-primary pull-right'>Add new species</a></td></tr>"
    $("#moded-species-table tbody").html(str);
}










