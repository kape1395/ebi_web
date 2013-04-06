
function ebi_init() {
    ebi_models_init();
    $("#ebi_moded").ebi_moded();
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
    $("#ebi_moded").show();
}



// =============================================================================
//  Main tabs: models tab
// =============================================================================

function ebi_models_init() {
    $("#main-tabs").on("click", "a[href='#model-list']", function () {
        ebi_model_list_load();
        ebi_model_list_show();
    });
    $("#main-tabs").on("click", "a[href='#model-new']", function() {
        $("#ebi_moded").ebi_moded("create");
        ebi_pages_show_moded();
    });
    $("#model-details").on("click", "a[href='#model-edit']", function() {
        var modelId = $("#model-details").data("modelid");
        $("#ebi_moded").ebi_moded("edit", modelId);
        ebi_pages_show_moded();
    });
    $("#model-details").on("click", "a[href='#model-copy']", function() {
        var modelId = $("#model-details").data("modelid");
        $("#ebi_moded").ebi_moded("copy", modelId);
        ebi_pages_show_moded();
    });
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

function ebi_model_show(modelId, modelRef) {
    $("#models > div").hide();
    $("#model-details").show();
    $("#model-details").data("modelid", modelId);
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
            str += "<td>" + ebi_mml_reaction_rate(r) + ",</td>";
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
            str += "<td>" + ebi_mml_reaction_rate(r) + ",</td>";
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


function ebi_mml_reaction_rate(r) {
    var str = "";
    if (r.type == "ebi_rdef_mm") {
        str += "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
        str += "    <mrow>";
        str += "        <mrow><mo>" + r.name + "</mo></mrow>";
        str += "        <mo>=</mo>";
        str += "        <mfrac>";
        str += "            <mrow><mi mathvariant='italic'>" + r.definition.vmax + "</mi><mi mathvariant='italic'>" + r.definition.substrate + "</mi></mrow>";
        str += "            <mrow><mi mathvariant='italic'>" + r.definition.km + "</mi><mo>+</mo><mi mathvariant='italic'>" + r.definition.substrate + "</mi></mrow>";
        str += "        </mfrac>";
        str += "    </mrow>";
        str += "</math>";
    } else if (r.type == "ebi_rdef_simple") {
        str += "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
        str += "    <mrow>";
        str += "        <mrow><mo>" + r.name + "</mo></mrow>";
        str += "        <mo>=</mo>";
        str += "        <mrow>";
        str += "<mi mathvariant='italic'>" + r.definition.rateconst + "</mi>";
        for (var j = 0; j < r.definition.reagents.length; j++) {
            str += "<mi mathvariant='italic'>" + r.definition.reagents[j].species + "</mi>";
        }
        str += "        </mrow>";
        str += "    </mrow>";
        str += "</math>";
    } else {
        str += "Unsupported";
    }
    return str;
}

