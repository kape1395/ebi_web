(function( $ ){

    // -------------------------------------------------------------------------
    //  Actions
    // -------------------------------------------------------------------------

    function do_init() {
        var $this = $(this);
        $this.load("ebi_moded.html", function () {
            $this.on("click", "a[href='#moded-general']",     function () {show_step($this, "moded-general");});
            $this.on("click", "a[href='#moded-species']",     function () {show_step($this, "moded-species");});
            $this.on("click", "a[href='#moded-reactions']",   function () {show_step($this, "moded-reactions");});
            $this.on("click", "a[href='#moded-domain']",      function () {show_step($this, "moded-domain");});
            $this.on("click", "a[href='#moded-parameters']",  function () {show_step($this, "moded-parameters");});
            $this.on("click", "a[href='#moded-done']",        function () {show_step($this, "moded-done");});
            $this.on("click", "a[href='#moded-cancel']",      ebi_pages_show_main_overview);    // TODO
            $this.on("click", "a[href='#moded-save']",        function () {save($this);});

            //
            //  General
            //
            $this.find(".moded-gen-name").change(function () {
                get_model($this).name = $(this).val();
            });
            $this.find(".moded-gen-desc").change(function () {
                get_model($this).description = $(this).val();
            });

            //
            //  Species
            //
            $this.on("change", ".moded-spc-name", function () {
                get_model($this).definition.species[$(this).closest("tr").data("idx")].name = $(this).val();
            });
            $this.on("change", ".moded-spc-desc", function () {
                get_model($this).definition.species[$(this).closest("tr").data("idx")].description = $(this).val();
            });
            $this.on("click", "a[href='#moded-species-add']", function () {
                get_model($this).definition.species.push({name:"", description:""});
                render_species($this);
            });
            $this.on("click", "a[href='#moded-species-rem']", function () {
                get_model($this).definition.species.splice($(this).closest("tr").data("idx"), 1);
                render_species($this);
            });

            //
            //  Reactions
            //
            $this.on("change", ".moded-rea-name", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].name = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-desc", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].description = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-mm-s", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.substrate = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-mm-p", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.product = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-mm-vmax", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.vmax = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-mm-km", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.km = $(this).val();
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-sm-r", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.reagents = spc_list_parse($(this).val());
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-sm-p", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.products = spc_list_parse($(this).val())
                render_reactions($this);
            });
            $this.on("change", ".moded-rea-sm-k", function () {
                get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.rateconst = $(this).val();
                render_reactions($this);
            });

            $this.on("click", "a[href='#moded-reaction-add-sm']", function () {
                get_model($this).definition.reactions.push({
                    name: "",
                    description: "",
                    type: "ebi_rdef_simple",
                    definition: {
                        reagents: [],
                        products: [],
                        rateconst: ""
                    }
                });
                render_reactions($this);
            });
            $this.on("click", "a[href='#moded-reaction-add-mm']", function () {
                get_model($this).definition.reactions.push({
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
                render_reactions($this);
            });
            $this.on("click", "a[href='#moded-reaction-rem']", function () {
                get_model($this).definition.reactions.splice($(this).closest("tr").data("idx"), 1);
                render_reactions($this);
            });

            //
            //  Domain / Compartments
            //
            AddCompFun = function (type) {
                get_model($this).definition.compartments.push({
                    name: "",
                    description: "",
                    type: type,
                    definition: {
                    }
                });
                render_domain($this);
            }
            $this.on("click", "a[href='#moded-comp-add-solution']",        function () { AddCompFun("ebi_cdef_solution"); });
            $this.on("click", "a[href='#moded-comp-add-diffusive']",       function () { AddCompFun("ebi_cdef_diffusive"); });
            $this.on("click", "a[href='#moded-comp-add-solid_electrode']", function () { AddCompFun("ebi_cdef_solid_electrode"); });
            $this.on("click", "a[href='#moded-comp-add-insulating']",      function () { AddCompFun("ebi_cdef_insulating"); });
            $this.on("click", "a[href='#moded-comp-rem']", function () {
                get_model($this).definition.compartments.splice($(this).closest("tr").data("idx"), 1);
                render_domain($this);
            });

            $this.on("change", ".moded-comp-diffusion", function () {
                var idx = $(this).closest("tr.moded_comp").data("idx");
                var comp = get_model($this).definition.compartments[idx];
                comp.definition.diffusion = $(this).val();
            });
            $this.on("click", "a[href='#moded-comp-species-expand']", function () {
                var mdef = get_model($this).definition;
                var comp = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")];
                var spcs = mdef.species;
                var cspc = [];
                for (var i = 0; i < spcs.length; i++) {
                    cspc.push({
                        species: spcs[i].name,
                        diffusion: comp.definition.diffusion,
                        concentration: 0
                    });
                }
                comp.definition.species = cspc;
                render_domain($this);
            });

            function unassign_spc_in_conds(cdef, spc) {
                if (cdef.conditions == undefined) {
                    cdef.conditions = [];
                }
                for (var i = 0; i < cdef.conditions.length; i++) {
                    var cnd = cdef.conditions[i];
                    for (var j = 0; j < cnd.species.length; j++) {
                        if (cnd.species[j] == spc) {
                            cnd.species.splice(j, 1);
                        }
                    }
                    if (cnd.species.length == 0) {
                        cdef.conditions.splice(i, 1);
                    }
                }
            }
            $this.on("click", "a[href='#moded-comp-diff-cnd']", function () {
                var mdef = get_model($this).definition;
                var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
                var spc  = $(this).data("spc");
                var cnd  = $(this).data("cnd");
                unassign_spc_in_conds(cdef, spc);
                cdef.conditions[cnd].species.push(spc);
                render_domain($this);
            });
            $this.on("click", "a[href='#moded-comp-diff-diff']", function () {
                var mdef = get_model($this).definition;
                var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
                var spc  = $(this).data("spc");
                unassign_spc_in_conds(cdef, spc);
                cdef.conditions.push({species: [spc], type: "diff", diffusion: 0});
                render_domain($this);
            });
            $this.on("click", "a[href='#moded-comp-diff-imob']", function () {
                var mdef = get_model($this).definition;
                var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
                var spc  = $(this).data("spc");
                var found = false;
                unassign_spc_in_conds(cdef, spc);
                for (var i = 0; i < cdef.conditions.length; i++) {
                    if (cdef.conditions[i].type == "imob") {
                        cdef.conditions[i].species.push(spc);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    cdef.conditions.push({species: [spc], type: "imob"});
                }
                render_domain($this);
            });
            $this.on("click", "a[href='#moded-comp-diff-none']", function () {
                var mdef = get_model($this).definition;
                var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
                var spc  = $(this).data("spc");
                unassign_spc_in_conds(cdef, spc);
                render_domain($this);
            });

            do_create.call($this);
        });
    }

    function do_create() {
        var $this = $(this);
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
        $this.find(".moded_mode").html("Creating new model.");
        set_model($this, model);
        render($this);
        show_general($this);
    }

    function do_edit(modelId) {
        var $this = $(this);
        $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
            $this.find(".moded_mode").html("Editing " + data.name + " (" + data.id + ").");
            set_model($this, data);
            render($this);
            show_general($this);
        });
    }

    function do_copy(modelId) {
        var $this = $(this);
        $.getJSON(api_url("model/" + modelId), function (data, textStatus, jqXHR) {
            $this.find(".moded_mode").html("Creating copy of " + data.name + " (" + data.id + ").");
            data.id = null;
            data.ref = null;
            data.name = "Copy of " + data.name;
            set_model($this, data);
            render($this);
            show_general($this);
        });
    }

    function get_model($this) {
        return $this.data("model");
    }

    function set_model($this, model) {
        $this.data("model", model);
    }

    // -------------------------------------------------------------------------
    //  Navigation
    // -------------------------------------------------------------------------

    function show_step($this, name) {
        $this.find("div.tab-content > div").hide();
        $this.find("div.tab-content > div." + name).show();
        $this.find("ul.breadcrumb a").removeClass("moded-current-step");
        $this.find("ul.breadcrumb a[href='#" + name + "']").addClass("moded-current-step");
    }

    function show_general($this) {
        show_step($this, "moded-general");
    }

    // -------------------------------------------------------------------------
    //  Rendering
    // -------------------------------------------------------------------------

    function render($this) {
        var model = get_model($this);
        $(".moded-gen-name").val(model.name);
        $(".moded-gen-desc").val(model.description);
        render_species($this);
        render_reactions($this);
        render_domain($this);
    }

    function render_species($this) {
        var model = get_model($this);
        var str = "";
        for (var i = 0; i < model.definition.species.length; i++) {
            var s = model.definition.species[i];
            str += "<tr data-idx='" + i + "'>";
            str += "<td><input type='text' placeholder='Species name' value='" + s.name + "' class='moded-spc-name'/></td>";
            str += "<td><input type='text' placeholder='Description' value='" + s.description + "' class='moded-spc-desc input-xlarge'/></td>";
            str += "<td><a href='#moded-species-rem' class='btn pull-right'>Remove</a></td>";
            str += "</tr>";
        }
        str += "<tr><td colspan='3'><a href='#moded-species-add' class='btn btn-primary pull-right'>Add new species</a></td></tr>";
        $this.find(".moded-species-table tbody").html(str);
    }

    function render_reactions($this) {
        var model = get_model($this);
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
                str += "  <div class='moded-reaction-simple'>";
                str += "    <input type='text' placeholder='Reagents' title='Example: 2 S1 + S2' class='reagent moded-rea-sm-r' value='" + spc_list_format(r.definition.reagents) + "'/>";
                str += "    <input type='text' placeholder='Products' title='Example: P'         class='product moded-rea-sm-p' value='" + spc_list_format(r.definition.products) + "'/>";
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
                str += "<div class='moded-reaction-michaelis'>";
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
        $this.find(".moded-reactions-table tbody").html(str);
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }

    function render_domain($this) {
        var model = get_model($this);
        var str = "";
        for (var i = 0; i < model.definition.compartments.length; i++) {
            var c = model.definition.compartments[i];
            str += "<tr data-idx='" + i + "' class='moded_comp'>";
            str += "<td>";
            str += "    " + c.type + "<br>";
            str += "    <input type='text' placeholder='Compartment name' value='" + c.name + "' class='moded-comp-name input-medium'/><br>";
            str += "    <textarea placeholder='Description' class='moded-comp-desc input-medium hidden-bydef'>" + c.description + "</textarea>";
            str += "</td>";
            str += "<td>";
            if (c.type == "ebi_cdef_solution") {
                str += render_compartment_species(model.definition.species, c);
            } else if (c.type == "ebi_cdef_diffusive") {
                str += render_compartment_species_v2(model.definition.species, c);
            } else if (c.type == "ebi_cdef_solid_electrode") {
                str += "solid_electrode...";
            } else if (c.type == "ebi_cdef_insulating") {
                str += "insulating...";
            } else {
                str += "Unsupported";
            }
            str += "</td>";
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
        $this.find(".moded-domain-table tbody").html(str);
        $this.find(".moded-domain-table tbody .hidden-bydef").hidden_bydef();
    }

    function render_compartment_species(species, compartment) {
        var spcs = compartment.definition.species;
        var str = "";
        str += "<div class='moded-comp-species'><table>";
        if (spcs != null && spcs != undefined && spcs.length > 0) {
            str += "<caption>Species</caption>";
            str += "<thead><tr><th>Name</th><th>Diff. coef.</th><th>Initial conc.</th></tr></thhead>";
            str += "<tbody>";
            for (var i = 0; i < species.length; i++) {
                var spc = null;
                for (var j = 0; j < spcs.length; j++) {
                    if (spcs[j].species == species[i].name) {
                        spc = spcs[j];
                        break;
                    }
                }
                str += "<tr><td>" + species[i].name + "</td>";
                if (spc != null) {
                    str += "<td>" + spc.diffusion + "</td>";
                    str += "<td>" + spc.concentration + "</td>";
                } else {
                    str += "<td>???</td>";
                    str += "<td>???</td>";
                }
                str += "</tr>";
            }
            str += "</tbody>";
        } else {
            str += "<caption>Species (<a href='#moded-comp-species-expand'>expand</a>)</caption>";
            str += "<tr><td>";
            str += "<label>Diffusion coeficient</label><input type='text' class='moded-comp-diffusion'>";
            str += "</td></tr>";
            str += "</tbody>";
        }
        str += "</tbody></table></div>";
        return str;
    }

    function render_compartment_species_v2(species, compartment) {
        var cnds = compartment.definition.conditions;
        var str = "";
        var selected;
        var other, snames;
        str += "<div class='moded-comp-cond'>";
        snames = [];
        for (var i = 0; i < species.length; i++) {
            snames.push(species[i].name);
        }
        if (cnds == null || cnds == undefined) {
            cnds = [];
        }

        function separator(index, length) {
            var s = "";
            if (index > 0 && index == length - 1) {
                s += ", ";  // " and ";
            } else if (index > 0) {
                s += ", ";
            }
            return s;
        }
        function species_dropdown(spcName, cndIndex) {
            var s = "";
            s += "<span class='dropdown'>";
            s += "<a href='#' class='dropdown-toggle' data-toggle='dropdown'>" + spcName + "</a>";
            s += "<ul class='dropdown-menu'>";
            for (var i = 0; i < cnds.length; i++) {
                if (cnds[i].type == "diff" && i != cndIndex) {
                    s += "<li><a data-spc='" + spcName + "' data-cnd='" + i;
                    s += "' href='#moded-comp-diff-cnd'>" + spcName + " has the same diffusion as ";
                    for (var j = 0; j < cnds[i].species.length; j++) {
                        s += separator(j, cnds[i].species.length);
                        s += "<b>" + cnds[i].species[j] + "</b>";
                    }
                    s += "</a></li>";
                }
            }
            s += "<li><a data-spc='" + spcName + "' href='#moded-comp-diff-diff'>" + spcName + " is affected by diffusion</a></li>";
            if (cndIndex == undefined || cnds[cndIndex].type != "imob") {
                s += "<li><a data-spc='" + spcName + "' href='#moded-comp-diff-imob'>" + spcName + " is immobilized</a></li>";
            }
            if (cndIndex != undefined) {
                s += "<li><a data-spc='" + spcName + "' href='#moded-comp-diff-none'>" + spcName + " is not present here</a></li>";
            }
            s += "</ul>";
            s += "</span>";
            return s;
        }

        str += "<div class='moded-comp-diff'>";
        str += "<table><caption>Species</caption>";
        str += "<tbody>";
        other = snames.slice(0);
        selected = [];
        for (var i = 0; i < cnds.length; i++) {
            var cnd = cnds[i];
            str += "<tr><td>";
            for (var j = 0; j < cnd.species.length; j++) {
                str += separator(j, cnd.species.length);
                str += species_dropdown(cnd.species[j], i);

                for (var k = 0; k < other.length; k++) {
                    if (other[k] == cnd.species[j]) {
                        other.splice(k, 1);
                    }
                }
                selected.push(cnd.species[j]);
            }
            str += "</td><td>";
            switch (cnd.type) {
            case "diff":
                str += " - afected by diffusion, coef. = <input type='text'>";
                break;
            case "imob":
                str += " - immobilized";
                break;
            default:
                str += "???";
            }
            str += "</td></tr>";
        }
        if (other.length > 0) {
            str += "<tr><td>";
            for (var j = 0; j < other.length; j++) {
                str += separator(j, other.length);
                str += species_dropdown(other[j], undefined);
            }
            str += "</td><td> - not presented here</td></tr>";
        }
        str += "</tbody></tbody></table></div>";

        str += "<span class='clearfix'></span></div>";
        return str;
    }

    // -------------------------------------------------------------------------
    //  Utilities
    // -------------------------------------------------------------------------

    function spc_list_format(list) {
        var out = "";
        for (var j = 0; j < list.length; j++) {
            var rr = list[j];
            if (j > 0) out += " + ";
            if (rr.number != 1) out += "" + rr.number + " ";
            out += rr.species;
        }
        return out;
    }

    function spc_list_parse(str) {
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

    // -------------------------------------------------------------------------
    //  Other
    // -------------------------------------------------------------------------

    function save($this) {
        var model = get_model($this);
        if (model.id == null) {
            $.ajax({
                type: "POST",
                url: api_url("model"),
                data: JSON.stringify(model),
                success: function () {
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
                    ebi_pages_show_main_overview();
                },
                dataType: "json"
            });
        }
    }

    // -------------------------------------------------------------------------
    //  JQuery stuff
    // -------------------------------------------------------------------------

    var methods = {
        init : function() {
            return this.each(function(i){
                do_init.call(this);
            });
         },
         create : function() {
             return this.each(function(){
                 do_create.call(this);
             });
         },
         edit : function( modelId ) {
             return this.each(function(){
                 do_edit.call(this, modelId);
             });
         },
         copy : function( modelId ) {
             return this.each(function(){
                 do_copy.call(this, modelId);
             });
         }
    };

    $.fn.ebi_moded = function( method ) {
        if ( methods[method] ) {
            return methods[method].apply( this, Array.prototype.slice.call( arguments, 1 ));
        } else if ( typeof method === 'object' || ! method ) {
            return methods.init.apply( this, arguments );
        } else {
            $.error('Method ' +  method + ' does not exist on jQuery.ebi_moded');
        }
    };

})(jQuery);