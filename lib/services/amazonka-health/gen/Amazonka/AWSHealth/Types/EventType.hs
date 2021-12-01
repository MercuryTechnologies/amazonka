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
-- Module      : Amazonka.AWSHealth.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventType where

import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the metadata about a type of event that is reported by AWS
-- Health. The @EventType@ shows the category, service, and the event type
-- code of the event. For example, an @issue@ might be the category, @EC2@
-- the service, and @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ the event type code.
--
-- You can use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventTypes.html DescribeEventTypes>
-- API operation to return this information about an event.
--
-- You can also use the Amazon CloudWatch Events console to create a rule
-- so that you can get notified or take action when AWS Health delivers a
-- specific event to your AWS account. For more information, see
-- <https://docs.aws.amazon.com/health/latest/ug/cloudwatch-events-health.html Monitor for AWS Health events with Amazon CloudWatch Events>
-- in the /AWS Health User Guide/.
--
-- /See:/ 'newEventType' smart constructor.
data EventType = EventType'
  { -- | The AWS service that is affected by the event. For example, @EC2@,
    -- @RDS@.
    service :: Prelude.Maybe Prelude.Text,
    -- | A list of event type category codes (@issue@, @scheduledChange@, or
    -- @accountNotification@).
    category :: Prelude.Maybe EventTypeCategory,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION @; for example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'eventType_service' - The AWS service that is affected by the event. For example, @EC2@,
-- @RDS@.
--
-- 'category', 'eventType_category' - A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
--
-- 'code', 'eventType_code' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
newEventType ::
  EventType
newEventType =
  EventType'
    { service = Prelude.Nothing,
      category = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The AWS service that is affected by the event. For example, @EC2@,
-- @RDS@.
eventType_service :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_service = Lens.lens (\EventType' {service} -> service) (\s@EventType' {} a -> s {service = a} :: EventType)

-- | A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
eventType_category :: Lens.Lens' EventType (Prelude.Maybe EventTypeCategory)
eventType_category = Lens.lens (\EventType' {category} -> category) (\s@EventType' {} a -> s {category = a} :: EventType)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
eventType_code :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_code = Lens.lens (\EventType' {code} -> code) (\s@EventType' {} a -> s {code = a} :: EventType)

instance Core.FromJSON EventType where
  parseJSON =
    Core.withObject
      "EventType"
      ( \x ->
          EventType'
            Prelude.<$> (x Core..:? "service")
            Prelude.<*> (x Core..:? "category")
            Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable EventType where
  hashWithSalt salt' EventType' {..} =
    salt' `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` service

instance Prelude.NFData EventType where
  rnf EventType' {..} =
    Prelude.rnf service `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf category
