(require 'org-latex)

(add-to-list 'org-export-latex-classes
             '("AnnArborbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{AnnArbor}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Antibesbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Antibes}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Bergenbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Bergen}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Berkeleybeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Berkeley}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Berlinbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Berlin}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Boadillabeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Boadilla}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("boxesbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{boxes}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("CambridgeUSbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{CambridgeUS}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Copenhagenbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Copenhagen}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Darmstadtbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Darmstadt}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("defaultbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{default}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Dresdenbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Dresden}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Frankfurtbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Frankfurt}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Goettingenbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Goettingen}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Hannoverbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Hannover}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Ilmenaubeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Ilmenau}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("JuanLesPinsbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{JuanLesPins}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Luebeckbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Luebeck}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Madridbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Madrid}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Malmoebeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Malmoe}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Marburgbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Marburg}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Montpellierbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Montpellier}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("PaloAltobeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{PaloAlto}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Pittsburghbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Pittsburgh}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Rochesterbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Rochester}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Singaporebeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Singapore}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Szegedbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Szeged}}"
               org-beamer-sectioning))
(add-to-list 'org-export-latex-classes
             '("Warsawbeamer"
               "\\documentclass[presentation]{beamer}
               \\mode<beamer>{\\usetheme{Warsaw}}"
               org-beamer-sectioning))
