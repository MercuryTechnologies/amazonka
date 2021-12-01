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
-- Module      : Amazonka.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Test where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a condition that is evaluated.
--
-- /See:/ 'newTest' smart constructor.
data Test = Test'
  { -- | The test\'s status.
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
    -- | The test\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | The test\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the test was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The test\'s stop time.
    stopped :: Prelude.Maybe Core.POSIX,
    -- | The test\'s result.
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
    -- | The test\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Represents the total (metered or unmetered) minutes used by the test.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | The test\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER
    --
    --     For Android, an app explorer that traverses an Android app,
    --     interacting with it and capturing screenshots at the same time.
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
    -- | A message about the test\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The test\'s start time.
    started :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Test' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'test_status' - The test\'s status.
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
-- 'counters', 'test_counters' - The test\'s result counters.
--
-- 'arn', 'test_arn' - The test\'s ARN.
--
-- 'created', 'test_created' - When the test was created.
--
-- 'stopped', 'test_stopped' - The test\'s stop time.
--
-- 'result', 'test_result' - The test\'s result.
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
-- 'name', 'test_name' - The test\'s name.
--
-- 'deviceMinutes', 'test_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
--
-- 'type'', 'test_type' - The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     For Android, an app explorer that traverses an Android app,
--     interacting with it and capturing screenshots at the same time.
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
-- 'message', 'test_message' - A message about the test\'s result.
--
-- 'started', 'test_started' - The test\'s start time.
newTest ::
  Test
newTest =
  Test'
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

-- | The test\'s status.
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
test_status :: Lens.Lens' Test (Prelude.Maybe ExecutionStatus)
test_status = Lens.lens (\Test' {status} -> status) (\s@Test' {} a -> s {status = a} :: Test)

-- | The test\'s result counters.
test_counters :: Lens.Lens' Test (Prelude.Maybe Counters)
test_counters = Lens.lens (\Test' {counters} -> counters) (\s@Test' {} a -> s {counters = a} :: Test)

-- | The test\'s ARN.
test_arn :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_arn = Lens.lens (\Test' {arn} -> arn) (\s@Test' {} a -> s {arn = a} :: Test)

-- | When the test was created.
test_created :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_created = Lens.lens (\Test' {created} -> created) (\s@Test' {} a -> s {created = a} :: Test) Prelude.. Lens.mapping Core._Time

-- | The test\'s stop time.
test_stopped :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_stopped = Lens.lens (\Test' {stopped} -> stopped) (\s@Test' {} a -> s {stopped = a} :: Test) Prelude.. Lens.mapping Core._Time

-- | The test\'s result.
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
test_result :: Lens.Lens' Test (Prelude.Maybe ExecutionResult)
test_result = Lens.lens (\Test' {result} -> result) (\s@Test' {} a -> s {result = a} :: Test)

-- | The test\'s name.
test_name :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_name = Lens.lens (\Test' {name} -> name) (\s@Test' {} a -> s {name = a} :: Test)

-- | Represents the total (metered or unmetered) minutes used by the test.
test_deviceMinutes :: Lens.Lens' Test (Prelude.Maybe DeviceMinutes)
test_deviceMinutes = Lens.lens (\Test' {deviceMinutes} -> deviceMinutes) (\s@Test' {} a -> s {deviceMinutes = a} :: Test)

-- | The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER
--
--     For Android, an app explorer that traverses an Android app,
--     interacting with it and capturing screenshots at the same time.
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
test_type :: Lens.Lens' Test (Prelude.Maybe TestType)
test_type = Lens.lens (\Test' {type'} -> type') (\s@Test' {} a -> s {type' = a} :: Test)

-- | A message about the test\'s result.
test_message :: Lens.Lens' Test (Prelude.Maybe Prelude.Text)
test_message = Lens.lens (\Test' {message} -> message) (\s@Test' {} a -> s {message = a} :: Test)

-- | The test\'s start time.
test_started :: Lens.Lens' Test (Prelude.Maybe Prelude.UTCTime)
test_started = Lens.lens (\Test' {started} -> started) (\s@Test' {} a -> s {started = a} :: Test) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Test where
  parseJSON =
    Core.withObject
      "Test"
      ( \x ->
          Test'
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

instance Prelude.Hashable Test where
  hashWithSalt salt' Test' {..} =
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

instance Prelude.NFData Test where
  rnf Test' {..} =
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
