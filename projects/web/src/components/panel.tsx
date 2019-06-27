import * as React from "react";

export function Panel({eventInfo}: any) {
  const [expanded, setExpanded] = React.useState(false);

  const toggle = () => {
    setExpanded(!expanded);
  };

  return (
    <div className="panel">
      <header className="row">
        <div className="column">
          <div className="flex-row">
            <div className="flex-grow">{eventInfo.eventTime}</div>
            <div>
              <a href="#" onClick={toggle}>
                <i className={"fa fa-fw fa-lg " + (expanded ? "fa-angle-up" : "fa-angle-down")}></i>
              </a>
            </div>
          </div>
        </div>
      </header>
      {expanded ?
        <section>
          <p>Event Type: {eventInfo.isA}</p>
          <p>Action: {eventInfo.action}</p>
        </section> : null}
    </div >
  );
}
