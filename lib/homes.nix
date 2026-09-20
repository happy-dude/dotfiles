# Helpers for facts that must hold across every profile.
#
# A disagreement names every profile and its value, so a shared fact cannot
# silently depend on which profile a check happens to select.
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
