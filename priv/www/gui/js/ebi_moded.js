(function( $ ){
    var compTypes = {
        "ebi_cdef_solution":        {desc: "Investigated solution", haveThns: true},
        "ebi_cdef_diffusive":       {desc: "Diffusive medium",      haveThns: true},
        "ebi_cdef_solid_electrode": {desc: "Solid electrode",       haveThns: false},
        "ebi_cdef_insulating":      {desc: "Insulating film",       haveThns: false}
    };

    // -------------------------------------------------------------------------
    //  Actions
    // -------------------------------------------------------------------------

    function do_init(returnFun) {
        var $this = $(this);
        $this.load("ebi_moded.html", function () {
            $this.on("click", "a[href='#moded-general']",     function () {show_step($this, "moded-general");});
            $this.on("click", "a[href='#moded-species']",     function () {show_step($this, "moded-species");});
            $this.on("click", "a[href='#moded-reactions']",   function () {show_step($this, "moded-reactions");});
            $this.on("click", "a[href='#moded-domain']",      function () {show_step($this, "moded-domain");});
            $this.on("click", "a[href='#moded-parameters']",  function () {show_step($this, "moded-parameters");});
            $this.on("click", "a[href='#moded-done']",        function () {show_step($this, "moded-done");});
            $this.on("click", "a[href='#moded-cancel']",      returnFun);
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

            reactions_init($this);
            domain_init($this);
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
            domain_cond_extract(data);
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
            domain_cond_extract(data);
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

    // -------------------------------------------------------------------------
    //  Species
    // -------------------------------------------------------------------------

    function render_species($this) {
        var model = get_model($this);
        var str = "";
        for (var i = 0; i < model.definition.species.length; i++) {
            var s = model.definition.species[i];
            str += "<tr data-idx='" + i + "'>";
            str += "<td><input type='text' placeholder='Species name' value='" + s.name + "' class='moded-spc-name span2'/></td>";
            str += "<td><input type='text' placeholder='Description' value='" + s.description + "' class='moded-spc-desc input-xlarge span6'/></td>";
            str += "<td><a href='#moded-species-rem' class='btn pull-right moded-species-rem'>&times;</a></td>";
            str += "</tr>";
        }
        str += "<tr><td colspan='3'><a href='#moded-species-add' class='btn btn-primary pull-right'>Add new species</a></td></tr>";
        $this.find(".moded-species-table tbody").html(str);
    }

    // -------------------------------------------------------------------------
    //  Reactions
    // -------------------------------------------------------------------------

    function reactions_init($this) {
        $this.on("change", ".moded-rea-name", function () {
            get_model($this).definition.reactions[$(this).closest("tr").data("idx")].name = $(this).val();
            render_reactions($this);
        });
        $this.on("change", ".moded-rea-desc", function () {
            get_model($this).definition.reactions[$(this).closest("tr").data("idx")].description = $(this).val();
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
        $this.on("change", ".moded-rea-fs-r", function () {
            get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.reagents = spc_list_parse($(this).val());
            render_reactions($this);
        });
        $this.on("change", ".moded-rea-fs-p", function () {
            get_model($this).definition.reactions[$(this).closest("tr").data("idx")].definition.products = spc_list_parse($(this).val())
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
        $this.on("click", "a[href='#moded-reaction-add-fs']", function () {
            get_model($this).definition.reactions.push({
                name: "",
                description: "",
                type: "ebi_rdef_fast",
                definition: {
                    reagents: [],
                    products: []
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
            } else if (r.type == "ebi_rdef_fast") {
                str += "<td>";
                str += "  <div class='moded-reaction-fast'>";
                str += "    <input type='text' placeholder='Reagents' title='Example: 2 S1 + S2' class='reagent moded-rea-fs-r' value='" + spc_list_format(r.definition.reagents) + "'/>";
                str += "    <input type='text' placeholder='Products' title='Example: P'         class='product moded-rea-fs-p' value='" + spc_list_format(r.definition.products) + "'/>";
                str += "    <div class='rate'>";
                str += "      <span>&infin;</span>";
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
        str += "      <li><a tabindex='-1' href='#moded-reaction-add-fs'>Fast</a></li>";
        str += "      <li><a tabindex='-1' href='#moded-reaction-add-mm'>Michaelis-Menten</a></li>";
        str += "    </ul>";
        str += "  </div>";
        str += "</td></tr>";
        $this.find(".moded-reactions-table tbody").html(str);
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }



    // -------------------------------------------------------------------------
    //  Domain / compartments
    // -------------------------------------------------------------------------

    function domain_init($this) {
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
        $this.on("click", "a[href='#moded-comp-remove']", function () {
            get_model($this).definition.compartments.splice($(this).closest("tr").data("idx"), 1);
            render_domain($this);
        });
        $this.on("click", "a[href='#moded-comp-moveup']", function () {
            var idx = $(this).closest("tr").data("idx");
            var cmps = get_model($this).definition.compartments;
            if (idx > 0) {
                var tmp = cmps[idx - 1];
                cmps[idx - 1] = cmps[idx];
                cmps[idx] = tmp;
            }
            render_domain($this);
        });
        $this.on("click", "a[href='#moded-comp-movedn']", function () {
            var idx = $(this).closest("tr").data("idx");
            var cmps = get_model($this).definition.compartments;
            if (idx < cmps.length - 1) {
                var tmp = cmps[idx + 1];
                cmps[idx + 1] = cmps[idx];
                cmps[idx] = tmp;
            }
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

        $this.on("click", "a[href='#moded-comp-reac-add']", function () {
            var mdef = get_model($this).definition;
            var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
            var reac = $(this).data("reac");
            var crs = cdef.reactions;
            if (crs == null || crs == undefined) {
                cdef.reactions = crs = [];
            }
            crs.push(reac);
            render_domain($this);
        });
        $this.on("click", "a[href='#moded-comp-reac-rem']", function () {
            var mdef = get_model($this).definition;
            var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
            var reac = $(this).data("reac");
            var crs = cdef.reactions;
            if (crs == null || crs == undefined) {
                cdef.reactions = crs = [];
            }
            for (var i = 0; i < crs.length; i++) {
                if (crs[i] == reac) {
                    crs.splice(i, 1);
                }
            }
            render_domain($this);
        });
        $this.on("click", "a[href='#moded-comp-solel-reac']", function () {
            var mdef = get_model($this).definition;
            var cdef = mdef.compartments[$(this).closest("tr.moded_comp").data("idx")].definition;
            var reac = $(this).data("reac");
            cdef.el_reaction = reac;
            render_domain($this);
        });
        $this.on("change", ".moded-comp-name", function () {
            var idx = $(this).closest("tr.moded_comp").data("idx");
            get_model($this).definition.compartments[idx].name = $(this).val();
        });
        $this.on("change", ".moded-comp-desc", function () {
            var idx = $(this).closest("tr.moded_comp").data("idx");
            get_model($this).definition.compartments[idx].description = $(this).val();
        });
        $this.on("change", ".moded-comp-thns", function () {
            var idx = $(this).closest("tr.moded_comp").data("idx");
            var cmp = get_model($this).definition.compartments[idx];
            switch (cmp.type) {
            case "ebi_cdef_solution":
                cmp.definition.nernst_thickness = $(this).val();
                break;
            case "ebi_cdef_diffusive":
                cmp.definition.thickness = $(this).val();
                break;
            }
        });
        $this.on("change", ".moded-comp-diff-diffcoef", function () {
            var cnd = $(this).data("cnd");
            var idx = $(this).closest("tr.moded_comp").data("idx");
            var cmp = get_model($this).definition.compartments[idx];
            cmp.definition.conditions[cnd].diffcoef = $(this).val();
        });
    }

    function domain_cond_extract(model) {
        console.log("domain_cond_extract: " + JSON.stringify(model));
        var cmps = model.definition.compartments;
        if (cmps == null || cmps == undefined) {
            cmps = model.definition.compartments = [];
        }
        for (var i = 0; i < cmps.length; i++) {
            var cdef = cmps[i].definition;
            var cnds = cdef.conditions = [];

            if (cdef.species == null || cdef.species == undefined) {
                cdef.species = [];
            }
            for (var j = 0; j < cdef.species.length; j++) {
                var spc = cdef.species[j];
                if (spc.diffusion == null) {
                    var found = false;
                    for (var k = 0; k < cnds.length; k++) {
                        if (cnds[k].type == "imob") {
                            cnds[k].species.push(spc.species);
                            found = true;
                        }
                    }
                    if (!found) {
                        cnds.push({type: "imob", species: [spc.species]});
                    }
                } else {
                    var found = false;
                    for (var k = 0; k < cnds.length; k++) {
                        if (cnds[k].type == "diff" && cnds[k].diffcoef == spc.diffusion) {
                            cnds[k].species.push(spc.species);
                            found = true;
                        }
                    }
                    if (!found) {
                        cnds.push({type: "diff", species: [spc.species], diffcoef: spc.diffusion});
                    }
                }
            }
            console.log("domain_cond_extract[" + i + "]: " + JSON.stringify(cdef));
        }
    }

    function domain_cond_apply(model) {
        console.log("domain_cond_apply: " + JSON.stringify(model));
        var cmps = model.definition.compartments;
        for (var i = 0; i < cmps.length; i++) {
            var cdef = cmps[i].definition;
            var spcs = cdef.species = [];

            if (cdef.conditions != null && cdef.conditions != undefined) {
                for (var j = 0; j < cdef.conditions.length; j++) {
                    var cnd = cdef.conditions[j];
                    for (var k = 0; k < cnd.species.length; k++) {
                        switch (cnd.type) {
                        case "diff":
                            spcs.push({species: cnd.species[k], diffusion: cnd.diffcoef, concentration: null});
                            break;
                        case "imob":
                            spcs.push({species: cnd.species[k], diffusion: null, concentration: null});
                        }
                    }
                }
                console.log("domain_cond_apply[" + i + "]: " + JSON.stringify(cdef));
            }
        }
    }

    function render_domain($this) {
        var model = get_model($this);
        var str = "";
        for (var i = 0; i < model.definition.compartments.length; i++) {
            var c = model.definition.compartments[i];
            str += "<tr data-idx='" + i + "' class='moded_comp'><td>";

            //
            // Compartment header column
            //
            str += "<div class='moded-comp-hdr moded-comp-block'>";
            str += "<table>";
            str += "<caption><span class='dropdown'>";
            str += "<a href='#' class='dropdown-toggle' data-toggle='dropdown'>" + compTypes[c.type].desc + " <b class='caret'></b></a>";
            str += "<ul class='dropdown-menu'>";
            str += "<li><a href='#moded-comp-remove'>Remove</a></li>";
            str += "<li><a href='#moded-comp-moveup'>Move up</a></li>";
            str += "<li><a href='#moded-comp-movedn'>Move down</a></li>";
            str += "</ul>";
            str += "</span></caption>";
            str += "<tbody>";
            str += "<tr><td>";
            str += "    <input type='text' placeholder='Compartment name' value='" + n(c.name) + "' class='moded-comp-name input-medium'/><br>";
            switch (c.type) {
            case 'ebi_cdef_solution':
                str += "<input type='text' placeholder='Nernst l. thickness' value='" + n(c.definition.nernst_thickness) + "' class='moded-comp-thns input-medium'/><br>";
                break;
            case 'ebi_cdef_diffusive':
                str += "<input type='text' placeholder='Layer thickness' value='" + n(c.definition.thickness) + "' class='moded-comp-thns input-medium'/><br>";
                break;
            }
            str += "    <textarea placeholder='Description' class='moded-comp-desc input-medium hidden-bydef'>" + n(c.description) + "</textarea>";
            str += "</td></tr>";
            str += "</tbody></tbody></table></div>";

            //
            //  Compartment parameters
            //
            if (c.type == "ebi_cdef_solution") {
                str += render_compartment_species(model.definition.species, c);
            } else if (c.type == "ebi_cdef_diffusive") {
                str += render_compartment_species(model.definition.species, c);
                str += render_compartment_reactions(model.definition.reactions, c);
            } else if (c.type == "ebi_cdef_solid_electrode") {
                str += render_compartment_solel_reaction(model.definition.reactions, c);
            } else if (c.type == "ebi_cdef_insulating") {
                //str += "insulating...";
            } else {
                str += "Unsupported compartment type";
            }
            str += "<span class='clearfix'></span></div>";
            str += "</td></tr>";
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
        var cnds = compartment.definition.conditions;
        var str = "";
        var selected;
        var other, snames;

        snames = [];
        for (var i = 0; i < species.length; i++) {
            snames.push(species[i].name);
        }
        if (cnds == null || cnds == undefined) {
            cnds = [];
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
            if ((cndIndex == undefined || cnds[cndIndex].type != "imob") && compartment.type == 'ebi_cdef_diffusive') {
                s += "<li><a data-spc='" + spcName + "' href='#moded-comp-diff-imob'>" + spcName + " is immobilized</a></li>";
            }
            if (cndIndex != undefined) {
                s += "<li><a data-spc='" + spcName + "' href='#moded-comp-diff-none'>" + spcName + " is not present here</a></li>";
            }
            s += "</ul>";
            s += "</span>";
            return s;
        }

        str += "<div class='moded-comp-diff moded-comp-block'>";
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
                str += " - afected by diffusion, coef. = <input type='text' class='moded-comp-diff-diffcoef' data-cnd='" + i + "' value='" + n(cnd.diffcoef) + "'>";
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
        return str;
    }

    function render_compartment_reactions(reactions, compartment) {
        var crs = compartment.definition.reactions;
        if (crs == null || crs == undefined) {
            crs = [];
        }

        var str = "";
        str += "<div class='moded-comp-reac moded-comp-block'><table>";
        str += "<caption><span class='dropdown'>";
        str += "<a href='#' class='dropdown-toggle' data-toggle='dropdown'>Reactions <b class='caret'></b></a>";
        str += "<ul class='dropdown-menu'>";
        for (var i = 0; i < reactions.length; i++) {
            var found = false;
            for (var j = 0; j < crs.length; j++) {
                if (crs[j] == reactions[i].name) {
                    found = true;
                    break;
                }
            }
            str += "<li><a data-reac='" + reactions[i].name + "'";
            if (found) {
                str += "href='#moded-comp-reac-rem'><b>" + reactions[i].name + "</b> is not takinging place here</a></li>";
            } else {
                str += "href='#moded-comp-reac-add'><b>" + reactions[i].name + "</b> takes place here</a></li>";
            }
        }
        str += "</ul>";
        str += "</span></caption>";
        str += "<tbody>";
        str += "<tr><td>";
        for (var i = 0; i < crs.length; i++) {
            str += separator(i, crs.length);
            str += crs[i];
        }
        str += "</td></tr>";
        str += "</tbody></tbody></table></div><span class='clearfix'></span></div>";
        return str;
    }

    function render_compartment_solel_reaction(reactions, compartment) {
        var str = "";
        str += "<div class='moded-comp-solel-reac moded-comp-block'><table>";
        str += "<caption><span class='dropdown'>";
        str += "<a href='#' class='dropdown-toggle' data-toggle='dropdown'>Electrochemical reaction <b class='caret'></b></a>";
        str += "<ul class='dropdown-menu'>";
        for (var i = 0; i < reactions.length; i++) {
            str += "<li><a data-reac='" + reactions[i].name + "'";
            str += "href='#moded-comp-solel-reac'>Output is generated due to <b>" + reactions[i].name + "</b></a></li>";
        }
        str += "</ul>";
        str += "</span></caption>";
        str += "<tbody>";
        str += "<tr><td>" + n(compartment.definition.el_reaction) + "</td></tr>";
        str += "</tbody></tbody></table></div><span class='clearfix'></span></div>";
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

    function separator(index, length) {
        var s = "";
        if (index > 0 && index == length - 1) {
            s += ", ";  // " and ";
        } else if (index > 0) {
            s += ", ";
        }
        return s;
    }

    function n(str) {
        if (str == undefined || str == null) {
            return "";
        }
        return str;
    }

    // -------------------------------------------------------------------------
    //  Other
    // -------------------------------------------------------------------------

    function save($this) {
        var model = get_model($this);
        domain_cond_apply(model);
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
        init : function(returnFun) {
            return this.each(function(i){
                do_init.call(this, returnFun);
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