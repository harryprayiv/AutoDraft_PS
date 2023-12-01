A working template for a Purs-nix Purescript Halogen app using Vite for bundling and serving.  

To get this up and running using nix, I like to load it in direnv but you could use 

```
git clone --branch template https://github.com/harryprayiv/AutoDraft_PS.git
cd AutoDraft_PS/
nix develop
purs-nix compile
vite --open
```
