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
-- Module      : Amazonka.Lambda.Types.FunctionEventInvokeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionEventInvokeConfig where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types.DestinationConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newFunctionEventInvokeConfig' smart constructor.
data FunctionEventInvokeConfig = FunctionEventInvokeConfig'
  { -- | The Amazon Resource Name (ARN) of the function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum age of a request that Lambda sends to a function for
    -- processing.
    maximumEventAgeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that the configuration was last updated.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | A destination for events after they have been sent to a function for
    -- processing.
    --
    -- __Destinations__
    --
    -- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
    --
    -- -   __Queue__ - The ARN of an SQS queue.
    --
    -- -   __Topic__ - The ARN of an SNS topic.
    --
    -- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
    destinationConfig :: Prelude.Maybe DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionArn', 'functionEventInvokeConfig_functionArn' - The Amazon Resource Name (ARN) of the function.
--
-- 'maximumEventAgeInSeconds', 'functionEventInvokeConfig_maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for
-- processing.
--
-- 'maximumRetryAttempts', 'functionEventInvokeConfig_maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- 'lastModified', 'functionEventInvokeConfig_lastModified' - The date and time that the configuration was last updated.
--
-- 'destinationConfig', 'functionEventInvokeConfig_destinationConfig' - A destination for events after they have been sent to a function for
-- processing.
--
-- __Destinations__
--
-- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
-- -   __Queue__ - The ARN of an SQS queue.
--
-- -   __Topic__ - The ARN of an SNS topic.
--
-- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
newFunctionEventInvokeConfig ::
  FunctionEventInvokeConfig
newFunctionEventInvokeConfig =
  FunctionEventInvokeConfig'
    { functionArn =
        Prelude.Nothing,
      maximumEventAgeInSeconds = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      destinationConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the function.
functionEventInvokeConfig_functionArn :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
functionEventInvokeConfig_functionArn = Lens.lens (\FunctionEventInvokeConfig' {functionArn} -> functionArn) (\s@FunctionEventInvokeConfig' {} a -> s {functionArn = a} :: FunctionEventInvokeConfig)

-- | The maximum age of a request that Lambda sends to a function for
-- processing.
functionEventInvokeConfig_maximumEventAgeInSeconds :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
functionEventInvokeConfig_maximumEventAgeInSeconds = Lens.lens (\FunctionEventInvokeConfig' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@FunctionEventInvokeConfig' {} a -> s {maximumEventAgeInSeconds = a} :: FunctionEventInvokeConfig)

-- | The maximum number of times to retry when the function returns an error.
functionEventInvokeConfig_maximumRetryAttempts :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
functionEventInvokeConfig_maximumRetryAttempts = Lens.lens (\FunctionEventInvokeConfig' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@FunctionEventInvokeConfig' {} a -> s {maximumRetryAttempts = a} :: FunctionEventInvokeConfig)

-- | The date and time that the configuration was last updated.
functionEventInvokeConfig_lastModified :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.UTCTime)
functionEventInvokeConfig_lastModified = Lens.lens (\FunctionEventInvokeConfig' {lastModified} -> lastModified) (\s@FunctionEventInvokeConfig' {} a -> s {lastModified = a} :: FunctionEventInvokeConfig) Prelude.. Lens.mapping Core._Time

-- | A destination for events after they have been sent to a function for
-- processing.
--
-- __Destinations__
--
-- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
-- -   __Queue__ - The ARN of an SQS queue.
--
-- -   __Topic__ - The ARN of an SNS topic.
--
-- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
functionEventInvokeConfig_destinationConfig :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe DestinationConfig)
functionEventInvokeConfig_destinationConfig = Lens.lens (\FunctionEventInvokeConfig' {destinationConfig} -> destinationConfig) (\s@FunctionEventInvokeConfig' {} a -> s {destinationConfig = a} :: FunctionEventInvokeConfig)

instance Core.FromJSON FunctionEventInvokeConfig where
  parseJSON =
    Core.withObject
      "FunctionEventInvokeConfig"
      ( \x ->
          FunctionEventInvokeConfig'
            Prelude.<$> (x Core..:? "FunctionArn")
            Prelude.<*> (x Core..:? "MaximumEventAgeInSeconds")
            Prelude.<*> (x Core..:? "MaximumRetryAttempts")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "DestinationConfig")
      )

instance Prelude.Hashable FunctionEventInvokeConfig where
  hashWithSalt salt' FunctionEventInvokeConfig' {..} =
    salt' `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` maximumRetryAttempts
      `Prelude.hashWithSalt` maximumEventAgeInSeconds
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData FunctionEventInvokeConfig where
  rnf FunctionEventInvokeConfig' {..} =
    Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf maximumEventAgeInSeconds
