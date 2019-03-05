import * as React from "react"

export function Panel() {
  const [expanded, setExpanded] = React.useState(false);

  const toggle = () => {
    setExpanded(!expanded);
  }

  return (
    <div className="panel">
      <header className="row">
        <div className="column">
          <div className="flex-row">
            <div className="flex-grow">Event 01 - 09:32:00, Status: Verified</div>
            <div>
              <a href="#" onClick={toggle}>
                <i className={"fa fa-fw fa-lg " + (expanded ? "fa-angle-up" : "fa-angle-down")}></i>
              </a>
            </div>
          </div>
        </div>
      </header>
      {expanded ? <section>Event Type: Aggregation </section> : null}
    </div >
  )
}
