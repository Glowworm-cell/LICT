#' @import reticulate
#' @import dplyr
#' @import stringr
#' @export

Install_module <- function() {
  reticulate::py_run_string('

import importlib
import subprocess

def install_module(module_name):
    try:
        # Try to import the specified module
        importlib.import_module(module_name)
    except ImportError:
        # If import fails, use pip to install the module
        subprocess.check_call(["pip", "install", module_name])

# List of modules to install
modules_to_install = ["anthropic", "google-generativeai", "pathlib", "textwrap", "ipython"]

# Install each module
for module in modules_to_install:
    install_module(module)
')
}

