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
-- Module      : Amazonka.Glue.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Job where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.ExecutionProperty
import Amazonka.Glue.Types.JobCommand
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a job definition.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The @JobCommand@ that runs this job.
    command :: Prelude.Maybe JobCommand,
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The last point in time when this job definition was modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    workerType :: Prelude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- jobs of type Spark.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    --
    -- Jobs that are created without specifying a Glue version default to Glue
    -- 0.9.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name you assign to this job definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry this job after a JobRun fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) allocated to runs of
    -- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
    -- is a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | For Glue version 1.0 or earlier jobs, using the standard worker type,
    -- the number of Glue data processing units (DPUs) that can be allocated
    -- when this job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
    --
    -- The value that can be allocated for @MaxCapacity@ depends on whether you
    -- are running a Python shell job, an Apache Spark ETL job, or an Apache
    -- Spark streaming ETL job:
    --
    -- -   When you specify a Python shell job
    --     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
    --     or 1 DPU. The default is 0.0625 DPU.
    --
    -- -   When you specify an Apache Spark ETL job
    --     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
    --     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
    --     100 DPUs. The default is 10 DPUs. This job type cannot have a
    --     fractional DPU allocation.
    --
    -- For Glue version 2.0 jobs, you cannot instead specify a
    -- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
    -- @Number of workers@.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The default arguments for this job, specified as name-value pairs.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that Glue itself consumes.
    --
    -- For information about how to specify and consume your own Job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that Glue consumes to set up
    -- your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
    -- topic in the developer guide.
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time and date that this job definition was created.
    createdOn :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfWorkers', 'job_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'command', 'job_command' - The @JobCommand@ that runs this job.
--
-- 'notificationProperty', 'job_notificationProperty' - Specifies configuration properties of a job notification.
--
-- 'lastModifiedOn', 'job_lastModifiedOn' - The last point in time when this job definition was modified.
--
-- 'connections', 'job_connections' - The connections used for this job.
--
-- 'workerType', 'job_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- 'securityConfiguration', 'job_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'glueVersion', 'job_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
--
-- 'nonOverridableArguments', 'job_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'role'', 'job_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
--
-- 'name', 'job_name' - The name you assign to this job definition.
--
-- 'logUri', 'job_logUri' - This field is reserved for future use.
--
-- 'maxRetries', 'job_maxRetries' - The maximum number of times to retry this job after a JobRun fails.
--
-- 'executionProperty', 'job_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'allocatedCapacity', 'job_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'maxCapacity', 'job_maxCapacity' - For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job, an Apache Spark ETL job, or an Apache
-- Spark streaming ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
--
-- 'timeout', 'job_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'defaultArguments', 'job_defaultArguments' - The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
--
-- 'description', 'job_description' - A description of the job.
--
-- 'createdOn', 'job_createdOn' - The time and date that this job definition was created.
newJob ::
  Job
newJob =
  Job'
    { numberOfWorkers = Prelude.Nothing,
      command = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      connections = Prelude.Nothing,
      workerType = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      nonOverridableArguments = Prelude.Nothing,
      role' = Prelude.Nothing,
      name = Prelude.Nothing,
      logUri = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      timeout = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      description = Prelude.Nothing,
      createdOn = Prelude.Nothing
    }

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
job_numberOfWorkers :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_numberOfWorkers = Lens.lens (\Job' {numberOfWorkers} -> numberOfWorkers) (\s@Job' {} a -> s {numberOfWorkers = a} :: Job)

-- | The @JobCommand@ that runs this job.
job_command :: Lens.Lens' Job (Prelude.Maybe JobCommand)
job_command = Lens.lens (\Job' {command} -> command) (\s@Job' {} a -> s {command = a} :: Job)

-- | Specifies configuration properties of a job notification.
job_notificationProperty :: Lens.Lens' Job (Prelude.Maybe NotificationProperty)
job_notificationProperty = Lens.lens (\Job' {notificationProperty} -> notificationProperty) (\s@Job' {} a -> s {notificationProperty = a} :: Job)

-- | The last point in time when this job definition was modified.
job_lastModifiedOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastModifiedOn = Lens.lens (\Job' {lastModifiedOn} -> lastModifiedOn) (\s@Job' {} a -> s {lastModifiedOn = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | The connections used for this job.
job_connections :: Lens.Lens' Job (Prelude.Maybe ConnectionsList)
job_connections = Lens.lens (\Job' {connections} -> connections) (\s@Job' {} a -> s {connections = a} :: Job)

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
job_workerType :: Lens.Lens' Job (Prelude.Maybe WorkerType)
job_workerType = Lens.lens (\Job' {workerType} -> workerType) (\s@Job' {} a -> s {workerType = a} :: Job)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
job_securityConfiguration :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_securityConfiguration = Lens.lens (\Job' {securityConfiguration} -> securityConfiguration) (\s@Job' {} a -> s {securityConfiguration = a} :: Job)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
job_glueVersion :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_glueVersion = Lens.lens (\Job' {glueVersion} -> glueVersion) (\s@Job' {} a -> s {glueVersion = a} :: Job)

-- | Non-overridable arguments for this job, specified as name-value pairs.
job_nonOverridableArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_nonOverridableArguments = Lens.lens (\Job' {nonOverridableArguments} -> nonOverridableArguments) (\s@Job' {} a -> s {nonOverridableArguments = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
job_role :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_role = Lens.lens (\Job' {role'} -> role') (\s@Job' {} a -> s {role' = a} :: Job)

-- | The name you assign to this job definition.
job_name :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

-- | This field is reserved for future use.
job_logUri :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_logUri = Lens.lens (\Job' {logUri} -> logUri) (\s@Job' {} a -> s {logUri = a} :: Job)

-- | The maximum number of times to retry this job after a JobRun fails.
job_maxRetries :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_maxRetries = Lens.lens (\Job' {maxRetries} -> maxRetries) (\s@Job' {} a -> s {maxRetries = a} :: Job)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
job_executionProperty :: Lens.Lens' Job (Prelude.Maybe ExecutionProperty)
job_executionProperty = Lens.lens (\Job' {executionProperty} -> executionProperty) (\s@Job' {} a -> s {executionProperty = a} :: Job)

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
job_allocatedCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_allocatedCapacity = Lens.lens (\Job' {allocatedCapacity} -> allocatedCapacity) (\s@Job' {} a -> s {allocatedCapacity = a} :: Job)

-- | For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job, an Apache Spark ETL job, or an Apache
-- Spark streaming ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
job_maxCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Double)
job_maxCapacity = Lens.lens (\Job' {maxCapacity} -> maxCapacity) (\s@Job' {} a -> s {maxCapacity = a} :: Job)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
job_timeout :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_timeout = Lens.lens (\Job' {timeout} -> timeout) (\s@Job' {} a -> s {timeout = a} :: Job)

-- | The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
job_defaultArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_defaultArguments = Lens.lens (\Job' {defaultArguments} -> defaultArguments) (\s@Job' {} a -> s {defaultArguments = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | A description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | The time and date that this job definition was created.
job_createdOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdOn = Lens.lens (\Job' {createdOn} -> createdOn) (\s@Job' {} a -> s {createdOn = a} :: Job) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "NumberOfWorkers")
            Prelude.<*> (x Core..:? "Command")
            Prelude.<*> (x Core..:? "NotificationProperty")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> (x Core..:? "Connections")
            Prelude.<*> (x Core..:? "WorkerType")
            Prelude.<*> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "GlueVersion")
            Prelude.<*> ( x Core..:? "NonOverridableArguments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LogUri")
            Prelude.<*> (x Core..:? "MaxRetries")
            Prelude.<*> (x Core..:? "ExecutionProperty")
            Prelude.<*> (x Core..:? "AllocatedCapacity")
            Prelude.<*> (x Core..:? "MaxCapacity")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> ( x Core..:? "DefaultArguments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CreatedOn")
      )

instance Prelude.Hashable Job where
  hashWithSalt salt' Job' {..} =
    salt' `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` allocatedCapacity
      `Prelude.hashWithSalt` executionProperty
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` logUri
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` nonOverridableArguments
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` numberOfWorkers

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf allocatedCapacity
      `Prelude.seq` Prelude.rnf executionProperty
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf nonOverridableArguments
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf notificationProperty
      `Prelude.seq` Prelude.rnf command
