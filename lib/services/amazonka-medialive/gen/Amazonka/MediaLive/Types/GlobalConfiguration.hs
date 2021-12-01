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
-- Module      : Amazonka.MediaLive.Types.GlobalConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.GlobalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.GlobalConfigurationInputEndAction
import Amazonka.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Amazonka.MediaLive.Types.GlobalConfigurationOutputLockingMode
import Amazonka.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Amazonka.MediaLive.Types.InputLossBehavior
import qualified Amazonka.Prelude as Prelude

-- | Global Configuration
--
-- /See:/ 'newGlobalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'
  { -- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
    -- MediaLive will attempt to synchronize the output of each pipeline to the
    -- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
    -- of each pipeline to the Unix epoch.
    outputLockingMode :: Prelude.Maybe GlobalConfigurationOutputLockingMode,
    -- | Settings for system actions when input is lost.
    inputLossBehavior :: Prelude.Maybe InputLossBehavior,
    -- | Value to set the initial audio gain for the Live Event.
    initialAudioGain :: Prelude.Maybe Prelude.Int,
    -- | Adjusts video input buffer for streams with very low video framerates.
    -- This is commonly set to enabled for music channels with less than one
    -- video frame per second.
    supportLowFramerateInputs :: Prelude.Maybe GlobalConfigurationLowFramerateInputs,
    -- | Indicates the action to take when the current input completes (e.g.
    -- end-of-file). When switchAndLoopInputs is configured the encoder will
    -- restart at the beginning of the first input. When \"none\" is configured
    -- the encoder will transcode either black, a solid color, or a user
    -- specified slate images per the \"Input Loss Behavior\" configuration
    -- until the next input switch occurs (which is controlled through the
    -- Channel Schedule API).
    inputEndAction :: Prelude.Maybe GlobalConfigurationInputEndAction,
    -- | Indicates whether the rate of frames emitted by the Live encoder should
    -- be paced by its system clock (which optionally may be locked to another
    -- source via NTP) or should be locked to the clock of the source that is
    -- providing the input stream.
    outputTimingSource :: Prelude.Maybe GlobalConfigurationOutputTimingSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputLockingMode', 'globalConfiguration_outputLockingMode' - Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
--
-- 'inputLossBehavior', 'globalConfiguration_inputLossBehavior' - Settings for system actions when input is lost.
--
-- 'initialAudioGain', 'globalConfiguration_initialAudioGain' - Value to set the initial audio gain for the Live Event.
--
-- 'supportLowFramerateInputs', 'globalConfiguration_supportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
--
-- 'inputEndAction', 'globalConfiguration_inputEndAction' - Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
--
-- 'outputTimingSource', 'globalConfiguration_outputTimingSource' - Indicates whether the rate of frames emitted by the Live encoder should
-- be paced by its system clock (which optionally may be locked to another
-- source via NTP) or should be locked to the clock of the source that is
-- providing the input stream.
newGlobalConfiguration ::
  GlobalConfiguration
newGlobalConfiguration =
  GlobalConfiguration'
    { outputLockingMode =
        Prelude.Nothing,
      inputLossBehavior = Prelude.Nothing,
      initialAudioGain = Prelude.Nothing,
      supportLowFramerateInputs = Prelude.Nothing,
      inputEndAction = Prelude.Nothing,
      outputTimingSource = Prelude.Nothing
    }

-- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
globalConfiguration_outputLockingMode :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationOutputLockingMode)
globalConfiguration_outputLockingMode = Lens.lens (\GlobalConfiguration' {outputLockingMode} -> outputLockingMode) (\s@GlobalConfiguration' {} a -> s {outputLockingMode = a} :: GlobalConfiguration)

-- | Settings for system actions when input is lost.
globalConfiguration_inputLossBehavior :: Lens.Lens' GlobalConfiguration (Prelude.Maybe InputLossBehavior)
globalConfiguration_inputLossBehavior = Lens.lens (\GlobalConfiguration' {inputLossBehavior} -> inputLossBehavior) (\s@GlobalConfiguration' {} a -> s {inputLossBehavior = a} :: GlobalConfiguration)

-- | Value to set the initial audio gain for the Live Event.
globalConfiguration_initialAudioGain :: Lens.Lens' GlobalConfiguration (Prelude.Maybe Prelude.Int)
globalConfiguration_initialAudioGain = Lens.lens (\GlobalConfiguration' {initialAudioGain} -> initialAudioGain) (\s@GlobalConfiguration' {} a -> s {initialAudioGain = a} :: GlobalConfiguration)

-- | Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
globalConfiguration_supportLowFramerateInputs :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationLowFramerateInputs)
globalConfiguration_supportLowFramerateInputs = Lens.lens (\GlobalConfiguration' {supportLowFramerateInputs} -> supportLowFramerateInputs) (\s@GlobalConfiguration' {} a -> s {supportLowFramerateInputs = a} :: GlobalConfiguration)

-- | Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
globalConfiguration_inputEndAction :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationInputEndAction)
globalConfiguration_inputEndAction = Lens.lens (\GlobalConfiguration' {inputEndAction} -> inputEndAction) (\s@GlobalConfiguration' {} a -> s {inputEndAction = a} :: GlobalConfiguration)

-- | Indicates whether the rate of frames emitted by the Live encoder should
-- be paced by its system clock (which optionally may be locked to another
-- source via NTP) or should be locked to the clock of the source that is
-- providing the input stream.
globalConfiguration_outputTimingSource :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationOutputTimingSource)
globalConfiguration_outputTimingSource = Lens.lens (\GlobalConfiguration' {outputTimingSource} -> outputTimingSource) (\s@GlobalConfiguration' {} a -> s {outputTimingSource = a} :: GlobalConfiguration)

instance Core.FromJSON GlobalConfiguration where
  parseJSON =
    Core.withObject
      "GlobalConfiguration"
      ( \x ->
          GlobalConfiguration'
            Prelude.<$> (x Core..:? "outputLockingMode")
            Prelude.<*> (x Core..:? "inputLossBehavior")
            Prelude.<*> (x Core..:? "initialAudioGain")
            Prelude.<*> (x Core..:? "supportLowFramerateInputs")
            Prelude.<*> (x Core..:? "inputEndAction")
            Prelude.<*> (x Core..:? "outputTimingSource")
      )

instance Prelude.Hashable GlobalConfiguration where
  hashWithSalt salt' GlobalConfiguration' {..} =
    salt' `Prelude.hashWithSalt` outputTimingSource
      `Prelude.hashWithSalt` inputEndAction
      `Prelude.hashWithSalt` supportLowFramerateInputs
      `Prelude.hashWithSalt` initialAudioGain
      `Prelude.hashWithSalt` inputLossBehavior
      `Prelude.hashWithSalt` outputLockingMode

instance Prelude.NFData GlobalConfiguration where
  rnf GlobalConfiguration' {..} =
    Prelude.rnf outputLockingMode
      `Prelude.seq` Prelude.rnf outputTimingSource
      `Prelude.seq` Prelude.rnf inputEndAction
      `Prelude.seq` Prelude.rnf supportLowFramerateInputs
      `Prelude.seq` Prelude.rnf initialAudioGain
      `Prelude.seq` Prelude.rnf inputLossBehavior

instance Core.ToJSON GlobalConfiguration where
  toJSON GlobalConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("outputLockingMode" Core..=)
              Prelude.<$> outputLockingMode,
            ("inputLossBehavior" Core..=)
              Prelude.<$> inputLossBehavior,
            ("initialAudioGain" Core..=)
              Prelude.<$> initialAudioGain,
            ("supportLowFramerateInputs" Core..=)
              Prelude.<$> supportLowFramerateInputs,
            ("inputEndAction" Core..=)
              Prelude.<$> inputEndAction,
            ("outputTimingSource" Core..=)
              Prelude.<$> outputTimingSource
          ]
      )
