import * as React from "react";

import { EventAction, EventBusinessStep, EventDisposition } from "../../epcis";
import { EventStateProps, LabelField, MappingSelect } from "./common";

export function ObjectEventForm({ eventState }: EventStateProps) {
  const [event, setEvent] = eventState;

  return (
    <fieldset>
      <LabelField eventState={eventState} updateFn={(e, v) => ({ ...e, epcList: [v] })} />

      <label>
        <span>Action</span>
        <MappingSelect
          mapping={EventAction}
          value={event.action}
          setValue={(x: string) => setEvent({ ...event, action: x })} />
      </label>

      <label >
        <span>Business Step </span>
        <MappingSelect
          mapping={EventBusinessStep}
          value={event.bizStep}
          setValue={(x: string) => setEvent({ ...event, bizStep: x })} />
      </label>

      <label>
        <span>Disposition </span>
        <MappingSelect
          mapping={EventDisposition}
          value={event.disposition}
          setValue={(x: string) => setEvent({ ...event, disposition: x })} />
      </label>

    </fieldset>
  );
}
