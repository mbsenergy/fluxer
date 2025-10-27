.onLoad <- function(libname, pkgname) {
    # Suppress startup messages for specific packages
    suppressPackageStartupMessages({
        require(data.table)
        require(crayon)
    })

    # Create a colorful and appealing message
    cat(crayon::bgGreen(" ⚡ fluxer package loaded! ⚡ \n"))
    cat(crayon::cyan("📦 Version: 0.9.8\n"))
    cat(crayon::cyan("👨‍💻 Authors: Flux Team\n"))
    cat(crayon::green("🚀 Ready to crunch some data!\n"))
    cat(crayon::green("🔍 Happy data hunting! 📊\n"))
}
