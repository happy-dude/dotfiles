# Helpers for facts that must hold across every profile.
#
# Checks repeatedly bound one value per profile and then asserted the two
# were equal. Naming the shared value once says the same thing, and reports
# which profiles disagreed rather than which assertion failed.
{lib}: {
  # Return the value every profile derives, or fail naming the disagreement.
  shared = homes: description: get: let
    perProfile = lib.mapAttrs (_: get) homes;
    distinct = lib.unique (lib.attrValues perProfile);
  in
    if lib.length distinct == 1
    then lib.head distinct
    else
      throw (
        "profiles disagree on ${description}: "
        + lib.concatStringsSep ", " (
          lib.mapAttrsToList (name: value: "${name} = ${toString value}") perProfile
        )
      );
}
