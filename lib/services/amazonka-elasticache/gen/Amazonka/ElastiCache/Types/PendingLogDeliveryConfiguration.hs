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
-- Module      : Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.DestinationType
import Amazonka.ElastiCache.Types.LogFormat
import Amazonka.ElastiCache.Types.LogType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The log delivery configurations being modified
--
-- /See:/ 'newPendingLogDeliveryConfiguration' smart constructor.
data PendingLogDeliveryConfiguration = PendingLogDeliveryConfiguration'
  { -- | Returns the log format, either JSON or TEXT
    logFormat :: Prelude.Maybe LogFormat,
    -- | Configuration details of either a CloudWatch Logs destination or Kinesis
    -- Data Firehose destination.
    destinationDetails :: Prelude.Maybe DestinationDetails,
    -- | Refers to <https://redis.io/commands/slowlog slow-log>.
    logType :: Prelude.Maybe LogType,
    -- | Returns the destination type, either CloudWatch Logs or Kinesis Data
    -- Firehose.
    destinationType :: Prelude.Maybe DestinationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingLogDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logFormat', 'pendingLogDeliveryConfiguration_logFormat' - Returns the log format, either JSON or TEXT
--
-- 'destinationDetails', 'pendingLogDeliveryConfiguration_destinationDetails' - Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
--
-- 'logType', 'pendingLogDeliveryConfiguration_logType' - Refers to <https://redis.io/commands/slowlog slow-log>.
--
-- 'destinationType', 'pendingLogDeliveryConfiguration_destinationType' - Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
newPendingLogDeliveryConfiguration ::
  PendingLogDeliveryConfiguration
newPendingLogDeliveryConfiguration =
  PendingLogDeliveryConfiguration'
    { logFormat =
        Prelude.Nothing,
      destinationDetails = Prelude.Nothing,
      logType = Prelude.Nothing,
      destinationType = Prelude.Nothing
    }

-- | Returns the log format, either JSON or TEXT
pendingLogDeliveryConfiguration_logFormat :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogFormat)
pendingLogDeliveryConfiguration_logFormat = Lens.lens (\PendingLogDeliveryConfiguration' {logFormat} -> logFormat) (\s@PendingLogDeliveryConfiguration' {} a -> s {logFormat = a} :: PendingLogDeliveryConfiguration)

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
pendingLogDeliveryConfiguration_destinationDetails :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationDetails)
pendingLogDeliveryConfiguration_destinationDetails = Lens.lens (\PendingLogDeliveryConfiguration' {destinationDetails} -> destinationDetails) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationDetails = a} :: PendingLogDeliveryConfiguration)

-- | Refers to <https://redis.io/commands/slowlog slow-log>.
pendingLogDeliveryConfiguration_logType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogType)
pendingLogDeliveryConfiguration_logType = Lens.lens (\PendingLogDeliveryConfiguration' {logType} -> logType) (\s@PendingLogDeliveryConfiguration' {} a -> s {logType = a} :: PendingLogDeliveryConfiguration)

-- | Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
pendingLogDeliveryConfiguration_destinationType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationType)
pendingLogDeliveryConfiguration_destinationType = Lens.lens (\PendingLogDeliveryConfiguration' {destinationType} -> destinationType) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationType = a} :: PendingLogDeliveryConfiguration)

instance Core.FromXML PendingLogDeliveryConfiguration where
  parseXML x =
    PendingLogDeliveryConfiguration'
      Prelude.<$> (x Core..@? "LogFormat")
      Prelude.<*> (x Core..@? "DestinationDetails")
      Prelude.<*> (x Core..@? "LogType")
      Prelude.<*> (x Core..@? "DestinationType")

instance
  Prelude.Hashable
    PendingLogDeliveryConfiguration
  where
  hashWithSalt
    salt'
    PendingLogDeliveryConfiguration' {..} =
      salt' `Prelude.hashWithSalt` destinationType
        `Prelude.hashWithSalt` logType
        `Prelude.hashWithSalt` destinationDetails
        `Prelude.hashWithSalt` logFormat

instance
  Prelude.NFData
    PendingLogDeliveryConfiguration
  where
  rnf PendingLogDeliveryConfiguration' {..} =
    Prelude.rnf logFormat
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf logType
      `Prelude.seq` Prelude.rnf destinationDetails
