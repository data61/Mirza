import * as React from "react";

export function Panel({eventInfo}: any) {
  const [expanded, setExpanded] = React.useState(false);

  const toggle = () => {
    setExpanded(!expanded);
  };

  const eventTime = new Date(eventInfo.eventTime);
  const bizStep = eventInfo.bizStep.split(':').pop();
  const disp = eventInfo.disposition.split(':').pop();
  const eventType = eventInfo.isA;
  const action = eventInfo.action;

  return (
    <div className="panel">
      <header className="row">
        <div className="column">
          <div className="flex-row">
            <div className="flex-grow">{eventTime.toDateString()}</div>
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
          {eventType ? <p>Event Type: {eventType}</p> : null}
          {action ? <p>Action: {action}</p> : null}
          {bizStep ? <p>Business Step: {bizStep}</p> : null}
          {disp ? <p>Disposition: {disp}</p> : null}
        </section> : null}
    </div >
  );
}
