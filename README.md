% WEB UI for EBI
% Karolis Petrauskas
% 2012-12-27

General
=======

The UI is based on HTML5, [JQuery](http://jquery.com/) and
[Twitter Bootstrap](http://twitter.github.com/bootstrap/)
on the client side and the YAWS web server on the server side.

Requirements
============

  * Plugable model editors.
  * Use dialogs for simplified model construction.

  * Dimensionless?
  * Latex generator.
  * Support "model cases" - same parameters with multiple models.
    Will it support comparison of multiple models?


Design
======

Main window structure:

  * Model database.
  * 


REST API
========

Resources
---------

`biosensor`
:   A list of `biosensor/{BiosensorId}`.


`biosensor/{BiosensorId}`
:   ...

        {
            id,
            ref,
            name,
            desc
        }

`biosensor/{BiosensorId}/model`
:   Same as the `model`, just filtered by BiosensorId.

`model`
:   ...

        {
            id,
            ref,
            name,
            desc
        }

`model/{ModelId}`
:   ...

        {
            id,
            ref,
            name,
            desc
        }

    Links:
      * <biosensor>; ref=biosensor
      * <definitions>; ref=definition


model/{ModelId}/definition/{ModelRef}
:   Model definition according to some meta-model.
    This entity represents particular model, without attributes specific to this software.
    Several media types should be supported here including: `EBI/Json`, `EBI/Erlang` and `SBML/XML`.
    Contents of this entity are dependent on the media type, provided in the request (`Accept`).
    Default media type is `application/vnd.ebi-model-v1+json; level=0` (see `model_definition` bellow).
    This resource is read-only, therefore only GET method is supported.
    Model definitions can be created or updated via the `model/{ModelId}` resource.

    The following links are returned together with the model definition:
      * </api/model/{ModelId}>; ref=model

    The following headers are also included:
      * Last-Modified

`simulation/{SimulationId}`
:   ...



Media type definition for `application/vnd.ebi-model-v1+json; level=0`
======================================================================

This media type follows the internal data model for EBI quite closely and should be considered
as a proprietary format. There are two root-level objects in this media-type: `model` and `model_definition`.
The `model` object includes `model_definition` and its meta-information (description, id, tags, etc.) while the
`model_def` stands for the actual model of the biosensor.

The `model` object is defined as follows:

    {
        id,
        ref,
        name,
        description,
        status,
        changed,
        changed_by,
        definition = model_definition(),    -- last version if the definition in this media type.
        parameters
    }

The `model_definition` object is defined as follows:

    {
        substances: [],
        reactions: []
    }



Media type definition for `application/vnd.ebi-simulation-v1+json; level=0`
===========================================================================


TODO: Describe.



Developer notes
===============

To run the application interactively, start the Erlang as follows:

    mkdir -p temp/data/yaws/www temp/data/mnesia
    env ERL_LIBS=deps erl -pa ebin

and then execute the following erlang statements:

    ebi_test_utils:configure("./temp").
    % ebi_test_utils:install_db().        % For the first startup only.
    ebi_test_utils:start_core().
    ebi_web_test_utils:configure("priv/yaws.conf").
    ebi_web_test_utils:start().
    % ...
    rr(ebi).
    ebi_store:add_model(#model{name="Model-1", description="First model", changed=now(), changed_by="kp", status=active}).
    % ...
    % l(ebi_web_model_json).
    % l(ebi_web_yaws_appmod).
    % init:stop().

