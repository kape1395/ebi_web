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
                str += "  <div class='model-reaction-simple'>";
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
        $this.find(".moded-reactions-table tbody").html(str);
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }

    function render_domain($this) {
        var model = get_model($this);
        var str = "";
        for (var i = 0; i < model.definition.compartments.length; i++) {
            var c = model.definition.compartments[i];
            str += "<tr data-idx='" + i + "'>";
            str += "<td>";
            str += "    " + c.type + "<br>";
            str += "    <input type='text' placeholder='Compartment name' value='" + c.name + "' class='moded-comp-name input-medium'/><br>";
            str += "    <textarea placeholder='Description' class='moded-comp-desc input-medium hidden-bydef'>" + c.description + "</textarea>";
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
        $this.find(".moded-domain-table tbody").html(str);
        $this.find(".moded-domain-table tbody .hidden-bydef").hidden_bydef();
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