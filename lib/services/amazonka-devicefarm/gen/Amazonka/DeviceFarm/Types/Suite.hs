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
-- Module      : Amazonka.DeviceFarm.Types.Suite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Suite where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of one or more tests.
--
-- /See:/ 'newSuite' smart constructor.
data Suite = Suite'
  { -- | The suite\'s status.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PENDING_CONCURRENCY
    --
    -- -   PENDING_DEVICE
    --
    -- -   PROCESSING
    --
    -- -   SCHEDULING
    --
    -- -   PREPARING
    --
    -- -   RUNNING
    --
    -- -   COMPLETED
    --
    -- -   STOPPING
    status :: Prelude.Maybe ExecutionStatus,
    -- | The suite\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | The suite\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the suite was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The suite\'s stop time.
    stopped :: Prelude.Maybe Core.POSIX,
    -- | The suite\'s result.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PASSED
    --
    -- -   WARNED
    --
    -- -   FAILED
    --
    -- -   SKIPPED
    --
    -- -   ERRORED
    --
    -- -   STOPPED
    result :: Prelude.Maybe ExecutionResult,
    -- | The suite\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Represents the total (metered or unmetered) minutes used by the test
    -- suite.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | The suite\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER
    --
    --     Only available for Android; an app explorer that traverses an
    --     Android app, interacting with it and capturing screenshots at the
    --     same time.
    --
    -- -   APPIUM_JAVA_JUNIT
    --
    -- -   APPIUM_JAVA_TESTNG
    --
    -- -   APPIUM_PYTHON
    --
    -- -   APPIUM_NODE
    --
    -- -   APPIUM_RUBY
    --
    -- -   APPIUM_WEB_JAVA_JUNIT
    --
    -- -   APPIUM_WEB_JAVA_TESTNG
    --
    -- -   APPIUM_WEB_PYTHON
    --
    -- -   APPIUM_WEB_NODE
    --
    -- -   APPIUM_WEB_RUBY
    --
    -- -   CALABASH
    --
    -- -   INSTRUMENTATION
    --
    -- -   UIAUTOMATION
    --
    -- -   UIAUTOMATOR
    --
    -- -   XCTEST
    --
    -- -   XCTEST_UI
    type' :: Prelude.Maybe TestType,
    -- | A message about the suite\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The suite\'s start time.
    started :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Suite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'suite_status' - The suite\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
--
-- 'counters', 'suite_counters' - The suite\'s result counters.
--
-- 'arn', 'suite_arn' - The suite\'s ARN.
--
-- 'created', 'suite_created' - When the suite was created.
--
-- 'stopped', 'suite_stopped' - The suite\'s stop time.
--
-- 'result', 'suite_result' - The suite\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
--
-- 'name', 'suite_name' - The suite\'s name.
--
-- 'deviceMinutes', 'suite_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test
-- suite.
--
-- 'type'', 'suite_type' - The suite\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     Only available for Android; an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
--
-- 'message', 'suite_message' - A message about the suite\'s result.
--
-- 'started', 'suite_started' - The suite\'s start time.
newSuite ::
  Suite
newSuite =
  Suite'
    { status = Prelude.Nothing,
      counters = Prelude.Nothing,
      arn = Prelude.Nothing,
      created = Prelude.Nothing,
      stopped = Prelude.Nothing,
      result = Prelude.Nothing,
      name = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing,
      type' = Prelude.Nothing,
      message = Prelude.Nothing,
      started = Prelude.Nothing
    }

-- | The suite\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
suite_status :: Lens.Lens' Suite (Prelude.Maybe ExecutionStatus)
suite_status = Lens.lens (\Suite' {status} -> status) (\s@Suite' {} a -> s {status = a} :: Suite)

-- | The suite\'s result counters.
suite_counters :: Lens.Lens' Suite (Prelude.Maybe Counters)
suite_counters = Lens.lens (\Suite' {counters} -> counters) (\s@Suite' {} a -> s {counters = a} :: Suite)

-- | The suite\'s ARN.
suite_arn :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_arn = Lens.lens (\Suite' {arn} -> arn) (\s@Suite' {} a -> s {arn = a} :: Suite)

-- | When the suite was created.
suite_created :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_created = Lens.lens (\Suite' {created} -> created) (\s@Suite' {} a -> s {created = a} :: Suite) Prelude.. Lens.mapping Core._Time

-- | The suite\'s stop time.
suite_stopped :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_stopped = Lens.lens (\Suite' {stopped} -> stopped) (\s@Suite' {} a -> s {stopped = a} :: Suite) Prelude.. Lens.mapping Core._Time

-- | The suite\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
suite_result :: Lens.Lens' Suite (Prelude.Maybe ExecutionResult)
suite_result = Lens.lens (\Suite' {result} -> result) (\s@Suite' {} a -> s {result = a} :: Suite)

-- | The suite\'s name.
suite_name :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_name = Lens.lens (\Suite' {name} -> name) (\s@Suite' {} a -> s {name = a} :: Suite)

-- | Represents the total (metered or unmetered) minutes used by the test
-- suite.
suite_deviceMinutes :: Lens.Lens' Suite (Prelude.Maybe DeviceMinutes)
suite_deviceMinutes = Lens.lens (\Suite' {deviceMinutes} -> deviceMinutes) (\s@Suite' {} a -> s {deviceMinutes = a} :: Suite)

-- | The suite\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     Only available for Android; an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
suite_type :: Lens.Lens' Suite (Prelude.Maybe TestType)
suite_type = Lens.lens (\Suite' {type'} -> type') (\s@Suite' {} a -> s {type' = a} :: Suite)

-- | A message about the suite\'s result.
suite_message :: Lens.Lens' Suite (Prelude.Maybe Prelude.Text)
suite_message = Lens.lens (\Suite' {message} -> message) (\s@Suite' {} a -> s {message = a} :: Suite)

-- | The suite\'s start time.
suite_started :: Lens.Lens' Suite (Prelude.Maybe Prelude.UTCTime)
suite_started = Lens.lens (\Suite' {started} -> started) (\s@Suite' {} a -> s {started = a} :: Suite) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Suite where
  parseJSON =
    Core.withObject
      "Suite"
      ( \x ->
          Suite'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "counters")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "stopped")
            Prelude.<*> (x Core..:? "result")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "deviceMinutes")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "message")
            Prelude.<*> (x Core..:? "started")
      )

instance Prelude.Hashable Suite where
  hashWithSalt salt' Suite' {..} =
    salt' `Prelude.hashWithSalt` started
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` deviceMinutes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` counters
      `Prelude.hashWithSalt` status

instance Prelude.NFData Suite where
  rnf Suite' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf started
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deviceMinutes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf counters
