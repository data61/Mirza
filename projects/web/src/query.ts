const moment = require('moment');
import {EventType} from "./epcis";

export interface QueryFieldProps {
  updateFn: (e: QueryForm, value: string) => QueryForm;
}

export interface QueryForm {
  DateRange: Date,
  EventType: string,
  Label: string,
}

export function queryForm(): QueryForm {
  return {
    DateRange: moment() as Date,
    EventType: EventType.Object,
    Label: "" as string,
  };
}
