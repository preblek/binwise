# Rebuild the homepage illustration from the package's simulated examples.
# Run from the package root. No research data are used.
local({
  pkgload::load_all('.', quiet = TRUE)
  smooth <- get('binwise_smooth', envir = asNamespace('binwise'))
  jump <- get('binwise_interaction', envir = asNamespace('binwise'))
  sg <- bin_diagnose(smooth, 'exposure', bins = 4, warn = FALSE)
  sf <- bin_interact(smooth, 'outcome', 'exposure', schemes = list(groups = sg))
  jg <- bin_diagnose(jump, 'x', method = 'custom',
                    breaks = c(min(jump$x), 0, max(jump$x)), warn = FALSE)
  jf <- bin_interact(jump, 'threshold_outcome', c('x','z'),
                    controls = 'control', schemes = list(groups = jg))
  sx <- data.frame(exposure = seq(min(smooth$exposure), max(smooth$exposure), length.out = 400))
  jx <- data.frame(x = seq(min(jump$x), max(jump$x), length.out = 400), z = 0, control = 0)
  sp <- predict(sf, sx); jp <- predict(jf, jx)
  dir.create('pkgdown/assets/images', recursive = TRUE, showWarnings = FALSE)
  draw <- function(x, p, membership, title, xlabel) {
    line <- p$estimate[p$model == 'continuous']; grouped <- p$estimate[p$model == 'groups']
    plot(x, line, type = 'n', ylim = range(c(line,grouped), finite = TRUE),
         main = title, xlab = xlabel, ylab = 'Predicted mean outcome', bty = 'l')
    abline(h = pretty(range(c(line,grouped))), col = '#edf1f1', lwd = 1)
    lines(x,line,col = '#126b68',lwd = 2.5)
    for (lev in unique(membership)) {
      use <- membership == lev
      lines(x[use], grouped[use], col = '#c45b35', lwd = 2.5, lty = 2)
    }
    legend('topleft', c('Original numbers','Grouped values'), col = c('#126b68','#c45b35'),
           lty = c(1,2), lwd = 2.5, bty = 'n', cex = .82)
  }
  for (stacked in c(FALSE, TRUE)) {
    filename <- if (stacked) 'home-comparison-mobile.png' else 'home-comparison.png'
    ragg::agg_png(file.path('pkgdown/assets/images', filename),
                 width = if (stacked) 800 else 1440,
                 height = if (stacked) 1100 else 650, res = 150)
    par(mfrow = if (stacked) c(2,1) else c(1,2),
        mar = c(4.8,4.5,3.5,1.2), oma = c(2.4,0,0,0), family = 'sans',
        cex = if (stacked) .95 else 1.05,
        col.axis = '#183538', col.lab = '#183538', col.main = '#183538')
    draw(sx$exposure, sp, cut(sx$exposure, sg$breaks, include.lowest=TRUE),
         'A steady change', 'Exposure')
    draw(jx$x, jp, jx$x > 0, 'A known jump', 'Distance from the boundary')
    mtext(if (stacked) 'Simulated examples. Fitted means without confidence bands.' else
          'Simulated examples. Fitted means without confidence bands. Other predictors fixed at zero in the right panel.',
          outer = TRUE, side = 1, line = .6, cex = .76, col = '#536569')
    dev.off()
  }
})
