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
-- Module      : Amazonka.DeviceFarm.Types.Run
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Run where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.BillingMethod
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.CustomerArtifactPaths
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.DevicePlatform
import Amazonka.DeviceFarm.Types.DeviceSelectionResult
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionResultCode
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.Location
import Amazonka.DeviceFarm.Types.NetworkProfile
import Amazonka.DeviceFarm.Types.Radios
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a test run on a set of devices with a given app package, test
-- parameters, and so on.
--
-- /See:/ 'newRun' smart constructor.
data Run = Run'
  { -- | Specifies the billing method for a test run: @metered@ or @unmetered@.
    -- If the parameter is not specified, the default value is @metered@.
    --
    -- If you have unmetered device slots, you must set this to @unmetered@ to
    -- use them. Otherwise, the run is counted toward metered device minutes.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Prelude.Maybe Prelude.Bool,
    -- | The run\'s status.
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
    -- | Output @CustomerArtifactPaths@ object for the test run.
    customerArtifactPaths :: Prelude.Maybe CustomerArtifactPaths,
    -- | For fuzz tests, this is the number of events, between 1 and 10000, that
    -- the UI fuzz test should perform.
    eventCount :: Prelude.Maybe Prelude.Int,
    -- | The run\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | The run\'s platform.
    --
    -- Allowed values include:
    --
    -- -   ANDROID
    --
    -- -   IOS
    platform :: Prelude.Maybe DevicePlatform,
    -- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
    -- Using the same seed value between tests ensures identical event
    -- sequences.
    seed :: Prelude.Maybe Prelude.Int,
    -- | Information about the radio states for the run.
    radios :: Prelude.Maybe Radios,
    -- | The run\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the location that is used for the run.
    location :: Prelude.Maybe Location,
    -- | When the run was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | Information about the locale that is used for the run.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the YAML-formatted test specification for the run.
    testSpecArn :: Prelude.Maybe Prelude.Text,
    -- | The run\'s stop time.
    stopped :: Prelude.Maybe Core.POSIX,
    -- | The run\'s result.
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
    -- | The number of minutes the job executes before it times out.
    jobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The total number of completed jobs.
    completedJobs :: Prelude.Maybe Prelude.Int,
    -- | Supporting field for the result field. Set only if @result@ is
    -- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
    -- package parsing failure.
    resultCode :: Prelude.Maybe ExecutionResultCode,
    -- | The run\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An app to upload or that has been uploaded.
    appUpload :: Prelude.Maybe Prelude.Text,
    -- | Read-only URL for an object in an S3 bucket where you can get the
    -- parsing results of the test package. If the test package doesn\'t parse,
    -- the reason why it doesn\'t parse appears in the file that this URL
    -- points to.
    parsingResultUrl :: Prelude.Maybe Prelude.Text,
    -- | The network profile being used for a test run.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | Represents the total (metered or unmetered) minutes used by the test
    -- run.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | The run\'s type.
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
    -- | A message about the run\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Device Farm console URL for the recording of the run.
    webUrl :: Prelude.Maybe Prelude.Text,
    -- | The total number of jobs for the run.
    totalJobs :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the device pool for the run.
    devicePoolArn :: Prelude.Maybe Prelude.Text,
    -- | The run\'s start time.
    started :: Prelude.Maybe Core.POSIX,
    -- | The results of a device filter used to select the devices for a test
    -- run.
    deviceSelectionResult :: Prelude.Maybe DeviceSelectionResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Run' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMethod', 'run_billingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have unmetered device slots, you must set this to @unmetered@ to
-- use them. Otherwise, the run is counted toward metered device minutes.
--
-- 'skipAppResign', 'run_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'status', 'run_status' - The run\'s status.
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
-- 'customerArtifactPaths', 'run_customerArtifactPaths' - Output @CustomerArtifactPaths@ object for the test run.
--
-- 'eventCount', 'run_eventCount' - For fuzz tests, this is the number of events, between 1 and 10000, that
-- the UI fuzz test should perform.
--
-- 'counters', 'run_counters' - The run\'s result counters.
--
-- 'platform', 'run_platform' - The run\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
--
-- 'seed', 'run_seed' - For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
-- Using the same seed value between tests ensures identical event
-- sequences.
--
-- 'radios', 'run_radios' - Information about the radio states for the run.
--
-- 'arn', 'run_arn' - The run\'s ARN.
--
-- 'location', 'run_location' - Information about the location that is used for the run.
--
-- 'created', 'run_created' - When the run was created.
--
-- 'locale', 'run_locale' - Information about the locale that is used for the run.
--
-- 'testSpecArn', 'run_testSpecArn' - The ARN of the YAML-formatted test specification for the run.
--
-- 'stopped', 'run_stopped' - The run\'s stop time.
--
-- 'result', 'run_result' - The run\'s result.
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
-- 'jobTimeoutMinutes', 'run_jobTimeoutMinutes' - The number of minutes the job executes before it times out.
--
-- 'completedJobs', 'run_completedJobs' - The total number of completed jobs.
--
-- 'resultCode', 'run_resultCode' - Supporting field for the result field. Set only if @result@ is
-- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
-- package parsing failure.
--
-- 'name', 'run_name' - The run\'s name.
--
-- 'appUpload', 'run_appUpload' - An app to upload or that has been uploaded.
--
-- 'parsingResultUrl', 'run_parsingResultUrl' - Read-only URL for an object in an S3 bucket where you can get the
-- parsing results of the test package. If the test package doesn\'t parse,
-- the reason why it doesn\'t parse appears in the file that this URL
-- points to.
--
-- 'networkProfile', 'run_networkProfile' - The network profile being used for a test run.
--
-- 'deviceMinutes', 'run_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the test
-- run.
--
-- 'type'', 'run_type' - The run\'s type.
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
-- 'message', 'run_message' - A message about the run\'s result.
--
-- 'webUrl', 'run_webUrl' - The Device Farm console URL for the recording of the run.
--
-- 'totalJobs', 'run_totalJobs' - The total number of jobs for the run.
--
-- 'devicePoolArn', 'run_devicePoolArn' - The ARN of the device pool for the run.
--
-- 'started', 'run_started' - The run\'s start time.
--
-- 'deviceSelectionResult', 'run_deviceSelectionResult' - The results of a device filter used to select the devices for a test
-- run.
newRun ::
  Run
newRun =
  Run'
    { billingMethod = Prelude.Nothing,
      skipAppResign = Prelude.Nothing,
      status = Prelude.Nothing,
      customerArtifactPaths = Prelude.Nothing,
      eventCount = Prelude.Nothing,
      counters = Prelude.Nothing,
      platform = Prelude.Nothing,
      seed = Prelude.Nothing,
      radios = Prelude.Nothing,
      arn = Prelude.Nothing,
      location = Prelude.Nothing,
      created = Prelude.Nothing,
      locale = Prelude.Nothing,
      testSpecArn = Prelude.Nothing,
      stopped = Prelude.Nothing,
      result = Prelude.Nothing,
      jobTimeoutMinutes = Prelude.Nothing,
      completedJobs = Prelude.Nothing,
      resultCode = Prelude.Nothing,
      name = Prelude.Nothing,
      appUpload = Prelude.Nothing,
      parsingResultUrl = Prelude.Nothing,
      networkProfile = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing,
      type' = Prelude.Nothing,
      message = Prelude.Nothing,
      webUrl = Prelude.Nothing,
      totalJobs = Prelude.Nothing,
      devicePoolArn = Prelude.Nothing,
      started = Prelude.Nothing,
      deviceSelectionResult = Prelude.Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have unmetered device slots, you must set this to @unmetered@ to
-- use them. Otherwise, the run is counted toward metered device minutes.
run_billingMethod :: Lens.Lens' Run (Prelude.Maybe BillingMethod)
run_billingMethod = Lens.lens (\Run' {billingMethod} -> billingMethod) (\s@Run' {} a -> s {billingMethod = a} :: Run)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
run_skipAppResign :: Lens.Lens' Run (Prelude.Maybe Prelude.Bool)
run_skipAppResign = Lens.lens (\Run' {skipAppResign} -> skipAppResign) (\s@Run' {} a -> s {skipAppResign = a} :: Run)

-- | The run\'s status.
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
run_status :: Lens.Lens' Run (Prelude.Maybe ExecutionStatus)
run_status = Lens.lens (\Run' {status} -> status) (\s@Run' {} a -> s {status = a} :: Run)

-- | Output @CustomerArtifactPaths@ object for the test run.
run_customerArtifactPaths :: Lens.Lens' Run (Prelude.Maybe CustomerArtifactPaths)
run_customerArtifactPaths = Lens.lens (\Run' {customerArtifactPaths} -> customerArtifactPaths) (\s@Run' {} a -> s {customerArtifactPaths = a} :: Run)

-- | For fuzz tests, this is the number of events, between 1 and 10000, that
-- the UI fuzz test should perform.
run_eventCount :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_eventCount = Lens.lens (\Run' {eventCount} -> eventCount) (\s@Run' {} a -> s {eventCount = a} :: Run)

-- | The run\'s result counters.
run_counters :: Lens.Lens' Run (Prelude.Maybe Counters)
run_counters = Lens.lens (\Run' {counters} -> counters) (\s@Run' {} a -> s {counters = a} :: Run)

-- | The run\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
run_platform :: Lens.Lens' Run (Prelude.Maybe DevicePlatform)
run_platform = Lens.lens (\Run' {platform} -> platform) (\s@Run' {} a -> s {platform = a} :: Run)

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test.
-- Using the same seed value between tests ensures identical event
-- sequences.
run_seed :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_seed = Lens.lens (\Run' {seed} -> seed) (\s@Run' {} a -> s {seed = a} :: Run)

-- | Information about the radio states for the run.
run_radios :: Lens.Lens' Run (Prelude.Maybe Radios)
run_radios = Lens.lens (\Run' {radios} -> radios) (\s@Run' {} a -> s {radios = a} :: Run)

-- | The run\'s ARN.
run_arn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_arn = Lens.lens (\Run' {arn} -> arn) (\s@Run' {} a -> s {arn = a} :: Run)

-- | Information about the location that is used for the run.
run_location :: Lens.Lens' Run (Prelude.Maybe Location)
run_location = Lens.lens (\Run' {location} -> location) (\s@Run' {} a -> s {location = a} :: Run)

-- | When the run was created.
run_created :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_created = Lens.lens (\Run' {created} -> created) (\s@Run' {} a -> s {created = a} :: Run) Prelude.. Lens.mapping Core._Time

-- | Information about the locale that is used for the run.
run_locale :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_locale = Lens.lens (\Run' {locale} -> locale) (\s@Run' {} a -> s {locale = a} :: Run)

-- | The ARN of the YAML-formatted test specification for the run.
run_testSpecArn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_testSpecArn = Lens.lens (\Run' {testSpecArn} -> testSpecArn) (\s@Run' {} a -> s {testSpecArn = a} :: Run)

-- | The run\'s stop time.
run_stopped :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_stopped = Lens.lens (\Run' {stopped} -> stopped) (\s@Run' {} a -> s {stopped = a} :: Run) Prelude.. Lens.mapping Core._Time

-- | The run\'s result.
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
run_result :: Lens.Lens' Run (Prelude.Maybe ExecutionResult)
run_result = Lens.lens (\Run' {result} -> result) (\s@Run' {} a -> s {result = a} :: Run)

-- | The number of minutes the job executes before it times out.
run_jobTimeoutMinutes :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_jobTimeoutMinutes = Lens.lens (\Run' {jobTimeoutMinutes} -> jobTimeoutMinutes) (\s@Run' {} a -> s {jobTimeoutMinutes = a} :: Run)

-- | The total number of completed jobs.
run_completedJobs :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_completedJobs = Lens.lens (\Run' {completedJobs} -> completedJobs) (\s@Run' {} a -> s {completedJobs = a} :: Run)

-- | Supporting field for the result field. Set only if @result@ is
-- @SKIPPED@. @PARSING_FAILED@ if the result is skipped because of test
-- package parsing failure.
run_resultCode :: Lens.Lens' Run (Prelude.Maybe ExecutionResultCode)
run_resultCode = Lens.lens (\Run' {resultCode} -> resultCode) (\s@Run' {} a -> s {resultCode = a} :: Run)

-- | The run\'s name.
run_name :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_name = Lens.lens (\Run' {name} -> name) (\s@Run' {} a -> s {name = a} :: Run)

-- | An app to upload or that has been uploaded.
run_appUpload :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_appUpload = Lens.lens (\Run' {appUpload} -> appUpload) (\s@Run' {} a -> s {appUpload = a} :: Run)

-- | Read-only URL for an object in an S3 bucket where you can get the
-- parsing results of the test package. If the test package doesn\'t parse,
-- the reason why it doesn\'t parse appears in the file that this URL
-- points to.
run_parsingResultUrl :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_parsingResultUrl = Lens.lens (\Run' {parsingResultUrl} -> parsingResultUrl) (\s@Run' {} a -> s {parsingResultUrl = a} :: Run)

-- | The network profile being used for a test run.
run_networkProfile :: Lens.Lens' Run (Prelude.Maybe NetworkProfile)
run_networkProfile = Lens.lens (\Run' {networkProfile} -> networkProfile) (\s@Run' {} a -> s {networkProfile = a} :: Run)

-- | Represents the total (metered or unmetered) minutes used by the test
-- run.
run_deviceMinutes :: Lens.Lens' Run (Prelude.Maybe DeviceMinutes)
run_deviceMinutes = Lens.lens (\Run' {deviceMinutes} -> deviceMinutes) (\s@Run' {} a -> s {deviceMinutes = a} :: Run)

-- | The run\'s type.
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
run_type :: Lens.Lens' Run (Prelude.Maybe TestType)
run_type = Lens.lens (\Run' {type'} -> type') (\s@Run' {} a -> s {type' = a} :: Run)

-- | A message about the run\'s result.
run_message :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_message = Lens.lens (\Run' {message} -> message) (\s@Run' {} a -> s {message = a} :: Run)

-- | The Device Farm console URL for the recording of the run.
run_webUrl :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_webUrl = Lens.lens (\Run' {webUrl} -> webUrl) (\s@Run' {} a -> s {webUrl = a} :: Run)

-- | The total number of jobs for the run.
run_totalJobs :: Lens.Lens' Run (Prelude.Maybe Prelude.Int)
run_totalJobs = Lens.lens (\Run' {totalJobs} -> totalJobs) (\s@Run' {} a -> s {totalJobs = a} :: Run)

-- | The ARN of the device pool for the run.
run_devicePoolArn :: Lens.Lens' Run (Prelude.Maybe Prelude.Text)
run_devicePoolArn = Lens.lens (\Run' {devicePoolArn} -> devicePoolArn) (\s@Run' {} a -> s {devicePoolArn = a} :: Run)

-- | The run\'s start time.
run_started :: Lens.Lens' Run (Prelude.Maybe Prelude.UTCTime)
run_started = Lens.lens (\Run' {started} -> started) (\s@Run' {} a -> s {started = a} :: Run) Prelude.. Lens.mapping Core._Time

-- | The results of a device filter used to select the devices for a test
-- run.
run_deviceSelectionResult :: Lens.Lens' Run (Prelude.Maybe DeviceSelectionResult)
run_deviceSelectionResult = Lens.lens (\Run' {deviceSelectionResult} -> deviceSelectionResult) (\s@Run' {} a -> s {deviceSelectionResult = a} :: Run)

instance Core.FromJSON Run where
  parseJSON =
    Core.withObject
      "Run"
      ( \x ->
          Run'
            Prelude.<$> (x Core..:? "billingMethod")
            Prelude.<*> (x Core..:? "skipAppResign")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "customerArtifactPaths")
            Prelude.<*> (x Core..:? "eventCount")
            Prelude.<*> (x Core..:? "counters")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "seed")
            Prelude.<*> (x Core..:? "radios")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "locale")
            Prelude.<*> (x Core..:? "testSpecArn")
            Prelude.<*> (x Core..:? "stopped")
            Prelude.<*> (x Core..:? "result")
            Prelude.<*> (x Core..:? "jobTimeoutMinutes")
            Prelude.<*> (x Core..:? "completedJobs")
            Prelude.<*> (x Core..:? "resultCode")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "appUpload")
            Prelude.<*> (x Core..:? "parsingResultUrl")
            Prelude.<*> (x Core..:? "networkProfile")
            Prelude.<*> (x Core..:? "deviceMinutes")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "message")
            Prelude.<*> (x Core..:? "webUrl")
            Prelude.<*> (x Core..:? "totalJobs")
            Prelude.<*> (x Core..:? "devicePoolArn")
            Prelude.<*> (x Core..:? "started")
            Prelude.<*> (x Core..:? "deviceSelectionResult")
      )

instance Prelude.Hashable Run where
  hashWithSalt salt' Run' {..} =
    salt' `Prelude.hashWithSalt` deviceSelectionResult
      `Prelude.hashWithSalt` started
      `Prelude.hashWithSalt` devicePoolArn
      `Prelude.hashWithSalt` totalJobs
      `Prelude.hashWithSalt` webUrl
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` deviceMinutes
      `Prelude.hashWithSalt` networkProfile
      `Prelude.hashWithSalt` parsingResultUrl
      `Prelude.hashWithSalt` appUpload
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resultCode
      `Prelude.hashWithSalt` completedJobs
      `Prelude.hashWithSalt` jobTimeoutMinutes
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` testSpecArn
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` radios
      `Prelude.hashWithSalt` seed
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` counters
      `Prelude.hashWithSalt` eventCount
      `Prelude.hashWithSalt` customerArtifactPaths
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` skipAppResign
      `Prelude.hashWithSalt` billingMethod

instance Prelude.NFData Run where
  rnf Run' {..} =
    Prelude.rnf billingMethod
      `Prelude.seq` Prelude.rnf deviceSelectionResult
      `Prelude.seq` Prelude.rnf started
      `Prelude.seq` Prelude.rnf devicePoolArn
      `Prelude.seq` Prelude.rnf totalJobs
      `Prelude.seq` Prelude.rnf webUrl
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deviceMinutes
      `Prelude.seq` Prelude.rnf networkProfile
      `Prelude.seq` Prelude.rnf parsingResultUrl
      `Prelude.seq` Prelude.rnf appUpload
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resultCode
      `Prelude.seq` Prelude.rnf completedJobs
      `Prelude.seq` Prelude.rnf jobTimeoutMinutes
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf testSpecArn
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf radios
      `Prelude.seq` Prelude.rnf seed
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf counters
      `Prelude.seq` Prelude.rnf eventCount
      `Prelude.seq` Prelude.rnf customerArtifactPaths
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf skipAppResign
