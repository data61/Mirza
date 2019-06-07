import * as React from "react";

import { EventType } from "../../epcis";

import { EventStateProps, MappingSelect } from "./common";
import { ObjectEventForm } from "./object-event";

export function EventForm({ eventState }: EventStateProps) {
  const [event, setEvent] = eventState;

  const eventTypeForm = () => {
    switch (event.isA) {
      case EventType.Object: return <ObjectEventForm eventState={eventState}></ObjectEventForm>;
      default: return <div></div>;
    }
  };

  const getDateTime = () => {
    const x = event.eventTime;
    const padLeft = (n: number) => n < 10 ? "0" + n : "" + n;

    return `${x.getFullYear()}-${padLeft(x.getMonth() + 1)}-${padLeft(x.getDate())}T${padLeft(x.getHours())}:${padLeft(x.getMinutes())}`;
  };
  const setDateTime = (x: string) => {
    setEvent({ ...event, eventTime: new Date(x) });
  };

  return (
    <form>
      <fieldset>
        <label>
          <span>Event Type</span>
          <MappingSelect
            mapping={EventType}
            value={event.isA}
            setValue={(x: string) => setEvent({ ...event, isA: x })} />
        </label>

        <label>
          <span>Event Time</span>
          <input type="datetime-local" value={getDateTime()} onChange={(e) => setDateTime(e.target.value)} />
        </label>
      </fieldset>

      {eventTypeForm()}
    </form>
  );
}
