(gay-event
 (version 1)
 (kind observation)
 (color ((domain strategy) (phase impression) (role generator) (sensitivity internal)))
 (refs ())
 (body ((summary "Competitor copy shifted toward open protocol language.")))
 (feedback ()))

(gay-event
 (version 1)
 (kind protention)
 (color ((domain strategy) (phase protention) (role generator) (sensitivity internal)))
 (refs ((observation "obs-competitor-open-protocol")))
 (body ((claim "Developer-led positioning will outperform compliance copy for top-of-funnel activation.")
        (confidence 0.66)
        (horizon "30d")
        (falsifier "Compliance page wins qualified pipeline by more than 20%")))
 (feedback ((requested (contradiction evidence experiment-design)) (due "30d"))))

(gay-event
 (version 1)
 (kind obstruction)
 (color ((domain engineering) (phase reafference) (role critic) (sensitivity internal) (glue obstructed)))
 (refs ((protention "pt-developer-led")))
 (body ((conflict "OSS positioning creates self-hosting expectations the product cannot yet satisfy.")
        (resolution-needed "Split acquisition model by developer activation vs enterprise procurement.")))
 (feedback ((requested (experiment decision)))))

(gay-event
 (version 1)
 (kind experiment)
 (color ((domain market) (phase action) (role coordinator) (sensitivity internal)))
 (refs ((protention "pt-developer-led") (obstruction "ob-self-hosting-expectation")))
 (body ((claim "A/B landing page test: OSS page vs compliance page.")
        (horizon "30d")
        (success-metric "qualified pipeline and activation separately")))
 (feedback ((requested (result score)))))

(gay-event
 (version 1)
 (kind decision)
 (color ((domain strategy) (phase retention) (role coordinator) (sensitivity internal)))
 (refs ((experiment "ex-landing-page-split")))
 (body ((claim "Maintain split messaging: OSS for developer activation, compliance for enterprise pipeline.")))
 (feedback ((requested (retrospective)))))
