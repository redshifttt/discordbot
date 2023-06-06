{
    description = "Hercules Discord Bot";

    inputs.nixpkgs.url = "github:NixOS/nixpkgs";

    outputs = { self, nixpkgs }@inputs:
        let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
        devShells.x86_64-linux.default =
            pkgs.mkShell {
                packages = with pkgs; [
                    python311
                    python311Packages.discordpy
                    python311Packages.requests
                    python311Packages.aiohttp
                    python311Packages.humanize
                ];
                shellHook = ''
                    echo "devshell for the Hercules Discord bot"
                    '';
            };
    };
}
