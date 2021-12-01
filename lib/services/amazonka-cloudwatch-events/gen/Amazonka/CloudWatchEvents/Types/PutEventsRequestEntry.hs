{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Types.PutEventsRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.PutEventsRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an event to be submitted.
--
-- /See:/ 'newPutEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
  { -- | The time stamp of the event, per
    -- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
    -- is provided, the time stamp of the
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
    -- call is used.
    time :: Prelude.Maybe Core.POSIX,
    -- | Free-form string used to decide what fields to expect in the event
    -- detail.
    detailType :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services resources, identified by Amazon Resource Name (ARN),
    -- which the event primarily concerns. Any number, including zero, may be
    -- present.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The name or ARN of the event bus to receive the event. Only the rules
    -- that are associated with this event bus are used to match the event. If
    -- you omit this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The source of the event.
    source :: Prelude.Maybe Prelude.Text,
    -- | An X-Ray trade header, which is an http header (X-Amzn-Trace-Id) that
    -- contains the trace-id associated with the event.
    --
    -- To learn more about X-Ray trace headers, see
    -- <https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader Tracing header>
    -- in the X-Ray Developer Guide.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | A valid JSON string. There is no other schema imposed. The JSON string
    -- may contain fields and nested subobjects.
    detail :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventsRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'time', 'putEventsRequestEntry_time' - The time stamp of the event, per
-- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
-- is provided, the time stamp of the
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- call is used.
--
-- 'detailType', 'putEventsRequestEntry_detailType' - Free-form string used to decide what fields to expect in the event
-- detail.
--
-- 'resources', 'putEventsRequestEntry_resources' - Amazon Web Services resources, identified by Amazon Resource Name (ARN),
-- which the event primarily concerns. Any number, including zero, may be
-- present.
--
-- 'eventBusName', 'putEventsRequestEntry_eventBusName' - The name or ARN of the event bus to receive the event. Only the rules
-- that are associated with this event bus are used to match the event. If
-- you omit this, the default event bus is used.
--
-- 'source', 'putEventsRequestEntry_source' - The source of the event.
--
-- 'traceHeader', 'putEventsRequestEntry_traceHeader' - An X-Ray trade header, which is an http header (X-Amzn-Trace-Id) that
-- contains the trace-id associated with the event.
--
-- To learn more about X-Ray trace headers, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader Tracing header>
-- in the X-Ray Developer Guide.
--
-- 'detail', 'putEventsRequestEntry_detail' - A valid JSON string. There is no other schema imposed. The JSON string
-- may contain fields and nested subobjects.
newPutEventsRequestEntry ::
  PutEventsRequestEntry
newPutEventsRequestEntry =
  PutEventsRequestEntry'
    { time = Prelude.Nothing,
      detailType = Prelude.Nothing,
      resources = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      source = Prelude.Nothing,
      traceHeader = Prelude.Nothing,
      detail = Prelude.Nothing
    }

-- | The time stamp of the event, per
-- <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339>. If no time stamp
-- is provided, the time stamp of the
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- call is used.
putEventsRequestEntry_time :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.UTCTime)
putEventsRequestEntry_time = Lens.lens (\PutEventsRequestEntry' {time} -> time) (\s@PutEventsRequestEntry' {} a -> s {time = a} :: PutEventsRequestEntry) Prelude.. Lens.mapping Core._Time

-- | Free-form string used to decide what fields to expect in the event
-- detail.
putEventsRequestEntry_detailType :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.Text)
putEventsRequestEntry_detailType = Lens.lens (\PutEventsRequestEntry' {detailType} -> detailType) (\s@PutEventsRequestEntry' {} a -> s {detailType = a} :: PutEventsRequestEntry)

-- | Amazon Web Services resources, identified by Amazon Resource Name (ARN),
-- which the event primarily concerns. Any number, including zero, may be
-- present.
putEventsRequestEntry_resources :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe [Prelude.Text])
putEventsRequestEntry_resources = Lens.lens (\PutEventsRequestEntry' {resources} -> resources) (\s@PutEventsRequestEntry' {} a -> s {resources = a} :: PutEventsRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the event bus to receive the event. Only the rules
-- that are associated with this event bus are used to match the event. If
-- you omit this, the default event bus is used.
putEventsRequestEntry_eventBusName :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.Text)
putEventsRequestEntry_eventBusName = Lens.lens (\PutEventsRequestEntry' {eventBusName} -> eventBusName) (\s@PutEventsRequestEntry' {} a -> s {eventBusName = a} :: PutEventsRequestEntry)

-- | The source of the event.
putEventsRequestEntry_source :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.Text)
putEventsRequestEntry_source = Lens.lens (\PutEventsRequestEntry' {source} -> source) (\s@PutEventsRequestEntry' {} a -> s {source = a} :: PutEventsRequestEntry)

-- | An X-Ray trade header, which is an http header (X-Amzn-Trace-Id) that
-- contains the trace-id associated with the event.
--
-- To learn more about X-Ray trace headers, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader Tracing header>
-- in the X-Ray Developer Guide.
putEventsRequestEntry_traceHeader :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.Text)
putEventsRequestEntry_traceHeader = Lens.lens (\PutEventsRequestEntry' {traceHeader} -> traceHeader) (\s@PutEventsRequestEntry' {} a -> s {traceHeader = a} :: PutEventsRequestEntry)

-- | A valid JSON string. There is no other schema imposed. The JSON string
-- may contain fields and nested subobjects.
putEventsRequestEntry_detail :: Lens.Lens' PutEventsRequestEntry (Prelude.Maybe Prelude.Text)
putEventsRequestEntry_detail = Lens.lens (\PutEventsRequestEntry' {detail} -> detail) (\s@PutEventsRequestEntry' {} a -> s {detail = a} :: PutEventsRequestEntry)

instance Prelude.Hashable PutEventsRequestEntry where
  hashWithSalt salt' PutEventsRequestEntry' {..} =
    salt' `Prelude.hashWithSalt` detail
      `Prelude.hashWithSalt` traceHeader
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` detailType
      `Prelude.hashWithSalt` time

instance Prelude.NFData PutEventsRequestEntry where
  rnf PutEventsRequestEntry' {..} =
    Prelude.rnf time `Prelude.seq` Prelude.rnf detail
      `Prelude.seq` Prelude.rnf traceHeader
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf detailType

instance Core.ToJSON PutEventsRequestEntry where
  toJSON PutEventsRequestEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Time" Core..=) Prelude.<$> time,
            ("DetailType" Core..=) Prelude.<$> detailType,
            ("Resources" Core..=) Prelude.<$> resources,
            ("EventBusName" Core..=) Prelude.<$> eventBusName,
            ("Source" Core..=) Prelude.<$> source,
            ("TraceHeader" Core..=) Prelude.<$> traceHeader,
            ("Detail" Core..=) Prelude.<$> detail
          ]
      )
