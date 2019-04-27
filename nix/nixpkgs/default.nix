let
  spec = builtins.fromJSON (builtins.readFile ./src.json);
  src = {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };

in 
  builtins.fetchTarball src
