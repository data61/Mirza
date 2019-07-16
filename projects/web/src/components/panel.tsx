import * as moment from "moment";
import * as React from "react";


import { EventBusinessStep, EventDisposition, EventEPCIS, lookupByValue } from "../epcis";

export function Panel({ event }: { event: EventEPCIS }) {
  const [expanded, setExpanded] = React.useState(false);

  const toggle = () => {
    setExpanded(!expanded);
  };

  const eventTime = new Date(event.eventTime);
  const eventType = event.isA;
  const action = event.action;

  return (
    <div className="panel">
      <header className="row">
        <div className="column">
          <div className="flex-row">
            <div className="flex-grow">
              {lookupByValue(EventBusinessStep, event.bizStep)} - {moment(eventTime).format('MMMM Do YYYY, h:mm:ss a')}
            </div>
            <div>
              <a onClick={toggle}>
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
          {event.bizStep ? <p>Business Step: {lookupByValue(EventBusinessStep, event.bizStep)}</p> : null}
          {event.disposition ? <p>Disposition: {lookupByValue(EventDisposition, event.disposition)}</p> : null}
        </section> : null}
    </div >
  );
}
