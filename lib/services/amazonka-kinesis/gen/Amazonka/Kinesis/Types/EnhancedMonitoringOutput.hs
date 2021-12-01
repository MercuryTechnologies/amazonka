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
-- Module      : Amazonka.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.EnhancedMonitoringOutput where

import qualified Amazonka.Core as Core
import Amazonka.Kinesis.Types.MetricsName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output for EnableEnhancedMonitoring and
-- DisableEnhancedMonitoring.
--
-- /See:/ 'newEnhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { -- | Represents the list of all the metrics that would be in the enhanced
    -- state after the operation.
    desiredShardLevelMetrics :: Prelude.Maybe [MetricsName],
    -- | Represents the current state of the metrics that are in the enhanced
    -- state before the operation.
    currentShardLevelMetrics :: Prelude.Maybe [MetricsName],
    -- | The name of the Kinesis data stream.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnhancedMonitoringOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredShardLevelMetrics', 'enhancedMonitoringOutput_desiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
--
-- 'currentShardLevelMetrics', 'enhancedMonitoringOutput_currentShardLevelMetrics' - Represents the current state of the metrics that are in the enhanced
-- state before the operation.
--
-- 'streamName', 'enhancedMonitoringOutput_streamName' - The name of the Kinesis data stream.
newEnhancedMonitoringOutput ::
  EnhancedMonitoringOutput
newEnhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { desiredShardLevelMetrics =
        Prelude.Nothing,
      currentShardLevelMetrics = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
enhancedMonitoringOutput_desiredShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_desiredShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {desiredShardLevelMetrics} -> desiredShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {desiredShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Lens.coerced

-- | Represents the current state of the metrics that are in the enhanced
-- state before the operation.
enhancedMonitoringOutput_currentShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_currentShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {currentShardLevelMetrics} -> currentShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {currentShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Kinesis data stream.
enhancedMonitoringOutput_streamName :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe Prelude.Text)
enhancedMonitoringOutput_streamName = Lens.lens (\EnhancedMonitoringOutput' {streamName} -> streamName) (\s@EnhancedMonitoringOutput' {} a -> s {streamName = a} :: EnhancedMonitoringOutput)

instance Core.FromJSON EnhancedMonitoringOutput where
  parseJSON =
    Core.withObject
      "EnhancedMonitoringOutput"
      ( \x ->
          EnhancedMonitoringOutput'
            Prelude.<$> ( x Core..:? "DesiredShardLevelMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "CurrentShardLevelMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StreamName")
      )

instance Prelude.Hashable EnhancedMonitoringOutput where
  hashWithSalt salt' EnhancedMonitoringOutput' {..} =
    salt' `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` currentShardLevelMetrics
      `Prelude.hashWithSalt` desiredShardLevelMetrics

instance Prelude.NFData EnhancedMonitoringOutput where
  rnf EnhancedMonitoringOutput' {..} =
    Prelude.rnf desiredShardLevelMetrics
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf currentShardLevelMetrics
