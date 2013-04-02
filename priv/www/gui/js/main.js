
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
    $("#main-tabs").on("click", "a[href='#model-new']", function() {
        ebi_moded_new();
    });
    $("#model-details").on("click", "a[href='#model-edit']", function() {
        var modelId = $("#model-details").data("modelid");
        ebi_moded_edit(modelId);
    });
    $("#model-details").on("click", "a[href='#model-copy']", function() {
        var modelId = $("#model-details").data("modelid");
        ebi_moded_copy(modelId);
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
            species: [{name:"", description:""}],
            reactions: [],
            compartments: []
        },
        parameters: []
    };
    $(".moded_mode").html("Creating new model.");
    ebi_moded_model_set(model);
    ebi_moded_render();
    ebi_moded_show_general();
}

function ebi_moded_edit(modelId) {
    $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
        $(".moded_mode").html("Editing " + data.name + " (" + data.id + ").");
        ebi_moded_model_set(data);
        ebi_moded_render();
        ebi_moded_show_general();
    });
}

function ebi_moded_copy(modelId) {
    $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
        $(".moded_mode").html("Creating copy of " + data.name + " (" + data.id + ").");
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
    $("#moded").on("click", "a[href='#moded-cancel']",      ebi_pages_show_main_overview);
    $("#moded").on("click", "a[href='#moded-save']",        ebi_moded_save);

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
    $("#moded").on("change", ".moded-spc-name", function () {
        ebi_moded_model().definition.species[$(this).closest("tr").data("idx")].name = $(this).val();
        //ebi_moded_render_species();
    });
    $("#moded").on("change", ".moded-spc-desc", function () {
        ebi_moded_model().definition.species[$(this).closest("tr").data("idx")].description = $(this).val();
        //ebi_moded_render_species();
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
    $("#moded").on("change", ".moded-rea-name", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].name = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-desc", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].description = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-mm-s", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.substrate = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-mm-p", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.product = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-mm-vmax", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.vmax = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-mm-km", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.km = $(this).val();
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-sm-r", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.reagents = ebi_moded_spc_list_parse($(this).val());
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-sm-p", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.products = ebi_moded_spc_list_parse($(this).val())
        ebi_moded_render_reactions();
    });
    $("#moded").on("change", ".moded-rea-sm-k", function () {
        ebi_moded_model().definition.reactions[$(this).closest("tr").data("idx")].definition.rateconst = $(this).val();
        ebi_moded_render_reactions();
    });

    $("#moded").on("click", "a[href='#moded-reaction-add-sm']", function () {
        ebi_moded_model().definition.reactions.push({
            name: "",
            description: "",
            type: "ebi_rdef_simple",
            definition: {
                reagents: [],
                products: [],
                rateconst: ""
            }
        });
        ebi_moded_render_reactions();
    });
    $("#moded").on("click", "a[href='#moded-reaction-add-mm']", function () {
        ebi_moded_model().definition.reactions.push({
            name: "",
            description: "",
            type: "ebi_rdef_mm",
            definition: {
                substrate: "",
                product: "",
                vmax: "",
                km: ""
            }
        });
        ebi_moded_render_reactions();
    });
    $("#moded").on("click", "a[href='#moded-reaction-rem']", function () {
        ebi_moded_model().definition.reactions.splice($(this).closest("tr").data("idx"), 1);
        ebi_moded_render_reactions();
    });

    //
    //  Domain / Compartments
    //
    AddCompFun = function (model, type) {
        model.definition.compartments.push({
            name: "",
            description: "",
            type: type,
            definition: {
            }
        });
        ebi_moded_render_domain();
    }
    $("#moded").on("click", "a[href='#moded-comp-add-solution']",        function () { AddCompFun(ebi_moded_model(), "ebi_cdef_solution"); });
    $("#moded").on("click", "a[href='#moded-comp-add-diffusive']",       function () { AddCompFun(ebi_moded_model(), "ebi_cdef_diffusive"); });
    $("#moded").on("click", "a[href='#moded-comp-add-solid_electrode']", function () { AddCompFun(ebi_moded_model(), "ebi_cdef_solid_electrode"); });
    $("#moded").on("click", "a[href='#moded-comp-add-insulating']",      function () { AddCompFun(ebi_moded_model(), "ebi_cdef_insulating"); });
    $("#moded").on("click", "a[href='#moded-comp-rem']", function () {
        ebi_moded_model().definition.compartments.splice($(this).closest("tr").data("idx"), 1);
        ebi_moded_render_domain();
    });
}

function ebi_moded_render() {
    var model = ebi_moded_model();
    $("#moded-gen-name").val(model.name);
    $("#moded-gen-desc").val(model.description);
    ebi_moded_render_species();
    ebi_moded_render_reactions();
    ebi_moded_render_domain();
}

function ebi_moded_render_species() {
    var model = ebi_moded_model();
    var str = "";
    for (var i = 0; i < model.definition.species.length; i++) {
        var s = model.definition.species[i];
        str += "<tr data-idx='" + i + "'>";
        str += "<td><input type='text' placeholder='Species name' value='" + s.name + "' class='moded-spc-name'/></td>";
        str += "<td><input type='text' placeholder='Description' value='" + s.description + "' class='moded-spc-desc input-xlarge'/></td>";
        str += "<td><a href='#moded-species-rem' class='btn pull-right'>Remove</a></td>";
        str += "</tr>";
    }
    str += "<tr><td colspan='3'><a href='#moded-species-add' class='btn btn-primary pull-right'>Add new species</a></td></tr>"
    $("#moded-species-table tbody").html(str);
}

function ebi_moded_render_reactions() {
    var model = ebi_moded_model();
    var str = "";
    for (var i = 0; i < model.definition.reactions.length; i++) {
        var r = model.definition.reactions[i];
        str += "<tr data-idx='" + i + "'>";
        str += "<td>";
        str += "    <input type='text' placeholder='Reaction name' value='" + r.name + "' class='moded-rea-name input-medium'/><br>";
        str += "    <textarea placeholder='Description' class='moded-rea-desc input-medium'>" + r.description + "</textarea>";
        str += "</td>";
        if (r.type == "ebi_rdef_simple") {
            str += "<td>";
            str += "  <div class='model-reaction-simple'>";
            str += "    <input type='text' placeholder='Reagents' title='Example: 2 S1 + S2' class='reagent moded-rea-sm-r' value='" + ebi_moded_spc_list_format(r.definition.reagents) + "'/>";
            str += "    <input type='text' placeholder='Products' title='Example: P'         class='product moded-rea-sm-p' value='" + ebi_moded_spc_list_format(r.definition.products) + "'/>";
            str += "    <div class='rate'>";
            str += "      <input type='text' placeholder='Rate const' title='Example: k1' class='moded-rea-sm-k' value='" + r.definition.rateconst + "'/>";
            str += "      <svg xmlns='http://www.w3.org/2000/svg' version='1.1'>";
            str += "        <defs>";
            str += "          <marker id='Triangle' viewBox='0 0 10 10' refX='9' refY='5' markerUnits='strokeWidth' markerWidth='10' markerHeight='5' orient='auto'>";
            str += "            <path d='M 0 0 L 10 5 L 0 10 L 7 5 z' style='fill:#999;stroke:#999;'/>";
            str += "          </marker>";
            str += "        </defs>";
            str += "        <line x1='5' y1='5' x2='95%' y2='5' style='stroke:#999;stroke-width:3' marker-end='url(#Triangle)'/>";
            str += "      </svg>";
            str += "    </div>";
            str += "    <div class='clearfix'></div>";
            str += "  </div>";
            str += "</td>";
            str += "<td>" + ebi_mml_reaction_rate(r) + "</td>";
        } else if (r.type == "ebi_rdef_mm") {
            str += "<td>";
            str += "<div class='model-reaction-michaelis'>";
            str += "  <table class=''><tbody><tr data-idx='" + i + "'>";
            str += "    <td class='text-right'>Substrate:</td>";
            str += "    <td><input type='text' placeholder='Substrate' title='Substrate, example: S1' value='" + r.definition.substrate + "' class='moded-rea-mm-s'/></td>";
            str += "    <td class='text-right'>Product:</td>";
            str += "    <td><input type='text' placeholder='Product' title='Product, example: P1' value='" + r.definition.product + "' class='moded-rea-mm-p'/></td>";
            str += "  </tr><tr data-idx='" + i + "'>";
            str += "    <td class='text-right'>V_max:</td>";
            str += "    <td><input type='text' placeholder='V_max' title='Maximal reaction speed (V_max), example: k1' value='" + r.definition.vmax + "' class='moded-rea-mm-vmax'/></td>";
            str += "    <td class='text-right'>K_M:</td>";
            str += "    <td><input type='text' placeholder='K_M' title='Michaelis constant (K_M), example: k2' value='" + r.definition.km + "' class='moded-rea-mm-km'/></td>";
            str += "  </tr></tbody></table>";
            str += "</div>";
            str += "</td>";
            str += "<td>" + ebi_mml_reaction_rate(r) + "</td>";
        } else {
            str += "<td>Unsupported</td>";
            str += "<td>Unsupported</td>";
        }
        str += "<td><a href='#moded-reaction-rem' class='btn pull-right'>Remove</a></td>";
        str += "</tr>";
    }
    str += "<tr><td colspan='5'>";
    str += "  <div class='dropdown pull-right'>";
    str += "    <a href='#moded-reaction-add' class='btn btn-primary dropdown-toggle' data-toggle='dropdown'>Add new reaction <b class='caret'></b></a>";
    str += "    <ul class='dropdown-menu' role='menu' aria-labelledby='dLabel'>";
    str += "      <li><a tabindex='-1' href='#moded-reaction-add-sm'>Simple</a></li>";
    str += "      <li><a tabindex='-1' href='#moded-reaction-add-mm'>Michaelis-Menten</a></li>";
    str += "    </ul>";
    str += "  </div>";
    str += "</td></tr>";
    $("#moded-reactions-table tbody").html(str);
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}

function ebi_moded_render_domain() {
    var model = ebi_moded_model();
    var str = "";
    for (var i = 0; i < model.definition.compartments.length; i++) {
        var c = model.definition.compartments[i];
        str += "<tr data-idx='" + i + "'>";
        str += "<td>";
        str += "    " + c.type + "<br>";
        str += "    <input type='text' placeholder='Compartment name' value='" + c.name + "' class='moded-comp-name input-medium'/><br>";
        str += "    <textarea placeholder='Description' class='moded-comp-desc input-medium'>" + c.description + "</textarea>";
        str += "</td>";
        if (c.type == "ebi_cdef_solution") {
            str += "<td>";
            str += "solution...";
            str += "</td>";
        } else if (c.type == "ebi_cdef_diffusive") {
            str += "<td>";
            str += "diffusive...";
            str += "</td>";
        } else if (c.type == "ebi_cdef_solid_electrode") {
            str += "<td>";
            str += "solid_electrode...";
            str += "</td>";
        } else if (c.type == "ebi_cdef_insulating") {
            str += "<td>";
            str += "insulating...";
            str += "</td>";
        } else {
            str += "<td>Unsupported</td>";
        }
        str += "<td><a href='#moded-comp-rem' class='btn pull-right'>Remove</a></td>";
        str += "</tr>";
    }
    str += "<tr><td colspan='5'>";
    str += "  <div class='dropdown pull-right'>";
    str += "    <a href='#moded-comp-add' class='btn btn-primary dropdown-toggle' data-toggle='dropdown'>Add new compartment <b class='caret'></b></a>";
    str += "    <ul class='dropdown-menu' role='menu' aria-labelledby='dLabel'>";
    str += "      <li><a tabindex='-1' href='#moded-comp-add-solution'        >Investigated solution</a></li>";
    str += "      <li><a tabindex='-1' href='#moded-comp-add-diffusive'       >Diffusive medium</a></li>";
    str += "      <li><a tabindex='-1' href='#moded-comp-add-solid_electrode' >Solid electrode</a></li>";
    str += "      <li><a tabindex='-1' href='#moded-comp-add-insulating'      >Insulating film</a></li>";
    str += "    </ul>";
    str += "  </div>";
    str += "</td></tr>";
    $("#moded-domain-table tbody").html(str);
}

function ebi_moded_spc_list_format(list) {
    var out = "";
    for (var j = 0; j < list.length; j++) {
        var rr = list[j];
        if (j > 0) out += " + ";
        if (rr.number != 1) out += "" + rr.number + " ";
        out += rr.species;
    }
    return out;
}

function ebi_moded_spc_list_parse(str) {
    var result = [];
    var blocks = str.split("+");
    for (var i = 0; i < blocks.length; i++) {
        var terms = blocks[i].match(/\S+/g);
        if (terms.length == 1) {
            result.push({species: terms[0], number: 1});
        } else if (terms.length == 2) {
            result.push({species: terms[1], number: terms[0]});
        } else {
            console.log("Unparseable block: " + blocks[i]);
        }
    }
    return result;
}


function ebi_moded_save() {
    var model = ebi_moded_model();
    if (model.id == null) {
        $.ajax({
            type: "POST",
            url: api_url("model"),
            data: JSON.stringify(model),
            success: function () {
                console.log("Model created.");
                ebi_pages_show_main_overview();
            },
            dataType: "json"
        });
    } else {
        $.ajax({
            type: "PUT",
            url: api_url("model/" + model.id),
            data: JSON.stringify(model),
            success: function () {
                console.log("Model updated.");
                ebi_pages_show_main_overview();
            },
            dataType: "json"
        });
    }
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

