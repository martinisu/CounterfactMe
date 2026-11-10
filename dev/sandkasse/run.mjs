// Kjoer CounterfactMe i webR i sandkassen.
// Bruk: node run.mjs <pakkemappe> <r-fil>  -> R-koden skriver til /out.txt
import { WebR } from 'webr';
import fs from 'fs'; import path from 'path';
const [pkg, rfile] = process.argv.slice(2);
const webR = new WebR({ baseUrl: new URL('./node_modules/webr/dist/', import.meta.url).pathname, interactive: false });
await webR.init();
await webR.FS.mkdir('/cfm'); await webR.FS.mkdir('/cfm/R'); await webR.FS.mkdir('/cfm/extdata');
for (const f of fs.readdirSync(path.join(pkg,'R'))) if (f.endsWith('.R'))
  await webR.FS.writeFile('/cfm/R/'+f, fs.readFileSync(path.join(pkg,'R',f)));
for (const f of fs.readdirSync(path.join(pkg,'inst','extdata'))) if (f.endsWith('.csv'))
  await webR.FS.writeFile('/cfm/extdata/'+f, fs.readFileSync(path.join(pkg,'inst','extdata',f)));
await webR.FS.writeFile('/cfm/test-coherence.R', fs.readFileSync(path.join(pkg,'tests','testthat','test-coherence.R')));
const init = `
.so <- base::system.file
system.file <- function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
  if (identical(package, "CounterfactMe")) { p <- c(...); if (!length(p)) return("/cfm")
    return(file.path("/cfm", paste(sub("^inst/", "", paste(p, collapse = "/")))))}
  .so(..., package = package, lib.loc = lib.loc, mustWork = mustWork) }
for (f in c("zzz.R","samplers.R","conditional.R","impossibility.R","counterfact_me.R",
            "constrained.R","narrate.R","print.R"))
  source(file.path("/cfm/R", f))
for (f in setdiff(list.files("/cfm/R", pattern = "[.]R$"),
     c("llm.R","CounterfactMe-package.R","zzz.R","samplers.R","conditional.R","impossibility.R",
       "counterfact_me.R","constrained.R","narrate.R","print.R")))
  source(file.path("/cfm/R", f))
.load_data()
`;
try {
  await webR.evalRVoid(init);
  await webR.evalRVoid(fs.readFileSync(rfile, 'utf8'));
  const out = await webR.FS.readFile('/out.txt');
  process.stdout.write(Buffer.from(out).toString('utf8'));
} catch (e) { console.error('R-FEIL:', e.message); }
process.exit(0);
