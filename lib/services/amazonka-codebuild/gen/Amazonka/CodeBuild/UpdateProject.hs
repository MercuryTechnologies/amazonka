{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.UpdateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of a build project.
module Amazonka.CodeBuild.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_secondaryArtifacts,
    updateProject_artifacts,
    updateProject_environment,
    updateProject_concurrentBuildLimit,
    updateProject_badgeEnabled,
    updateProject_secondarySourceVersions,
    updateProject_queuedTimeoutInMinutes,
    updateProject_cache,
    updateProject_secondarySources,
    updateProject_sourceVersion,
    updateProject_vpcConfig,
    updateProject_source,
    updateProject_logsConfig,
    updateProject_fileSystemLocations,
    updateProject_buildBatchConfig,
    updateProject_encryptionKey,
    updateProject_description,
    updateProject_serviceRole,
    updateProject_tags,
    updateProject_timeoutInMinutes,
    updateProject_name,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | An array of @ProjectArtifact@ objects.
    secondaryArtifacts :: Prelude.Maybe [ProjectArtifacts],
    -- | Information to be changed about the build output artifacts for the build
    -- project.
    artifacts :: Prelude.Maybe ProjectArtifacts,
    -- | Information to be changed about the build environment for the build
    -- project.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | The maximum number of concurrent builds that are allowed for this
    -- project.
    --
    -- New builds are only started if the current number of builds is less than
    -- or equal to this limit. If the current build count meets this limit, new
    -- builds are throttled and are not run.
    --
    -- To remove this limit, set this value to -1.
    concurrentBuildLimit :: Prelude.Maybe Prelude.Int,
    -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
    -- is specified at the build level, then they take over these
    -- @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Stores recently used information so that it can be quickly accessed at a
    -- later time.
    cache :: Prelude.Maybe ProjectCache,
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | A version of the build input to be built for this project. If not
    -- specified, the latest version is used. If specified, it must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- -   For Bitbucket: the commit ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a branch name is specified, the branch\'s HEAD commit ID is used. If
    --     not specified, the default branch\'s HEAD commit ID is used.
    --
    -- -   For Amazon S3: the version ID of the object that represents the
    --     build input ZIP file to use.
    --
    -- If @sourceVersion@ is specified at the build level, then that version
    -- takes precedence over this @sourceVersion@ (at the project level).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Information to be changed about the build input source code for the
    -- build project.
    source :: Prelude.Maybe ProjectSource,
    -- | Information about logs for the build project. A project can create logs
    -- in CloudWatch Logs, logs in an S3 bucket, or both.
    logsConfig :: Prelude.Maybe LogsConfig,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    buildBatchConfig :: Prelude.Maybe ProjectBuildBatchConfig,
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | A new or replacement description of the build project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The replacement ARN of the IAM role that enables CodeBuild to interact
    -- with dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | An updated list of tag key and value pairs associated with this build
    -- project.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild build project tags.
    tags :: Prelude.Maybe [Tag],
    -- | The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
    -- to wait before timing out any related build that did not get marked as
    -- completed.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The name of the build project.
    --
    -- You cannot change a build project\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondaryArtifacts', 'updateProject_secondaryArtifacts' - An array of @ProjectArtifact@ objects.
--
-- 'artifacts', 'updateProject_artifacts' - Information to be changed about the build output artifacts for the build
-- project.
--
-- 'environment', 'updateProject_environment' - Information to be changed about the build environment for the build
-- project.
--
-- 'concurrentBuildLimit', 'updateProject_concurrentBuildLimit' - The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- To remove this limit, set this value to -1.
--
-- 'badgeEnabled', 'updateProject_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
--
-- 'secondarySourceVersions', 'updateProject_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
--
-- 'queuedTimeoutInMinutes', 'updateProject_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'cache', 'updateProject_cache' - Stores recently used information so that it can be quickly accessed at a
-- later time.
--
-- 'secondarySources', 'updateProject_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'sourceVersion', 'updateProject_sourceVersion' - A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'vpcConfig', 'updateProject_vpcConfig' - VpcConfig enables CodeBuild to access resources in an Amazon VPC.
--
-- 'source', 'updateProject_source' - Information to be changed about the build input source code for the
-- build project.
--
-- 'logsConfig', 'updateProject_logsConfig' - Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, logs in an S3 bucket, or both.
--
-- 'fileSystemLocations', 'updateProject_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'buildBatchConfig', 'updateProject_buildBatchConfig' - Undocumented member.
--
-- 'encryptionKey', 'updateProject_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'description', 'updateProject_description' - A new or replacement description of the build project.
--
-- 'serviceRole', 'updateProject_serviceRole' - The replacement ARN of the IAM role that enables CodeBuild to interact
-- with dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
--
-- 'tags', 'updateProject_tags' - An updated list of tag key and value pairs associated with this build
-- project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
--
-- 'timeoutInMinutes', 'updateProject_timeoutInMinutes' - The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
-- to wait before timing out any related build that did not get marked as
-- completed.
--
-- 'name', 'updateProject_name' - The name of the build project.
--
-- You cannot change a build project\'s name.
newUpdateProject ::
  -- | 'name'
  Prelude.Text ->
  UpdateProject
newUpdateProject pName_ =
  UpdateProject'
    { secondaryArtifacts =
        Prelude.Nothing,
      artifacts = Prelude.Nothing,
      environment = Prelude.Nothing,
      concurrentBuildLimit = Prelude.Nothing,
      badgeEnabled = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      cache = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      source = Prelude.Nothing,
      logsConfig = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      buildBatchConfig = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      description = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      name = pName_
    }

-- | An array of @ProjectArtifact@ objects.
updateProject_secondaryArtifacts :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectArtifacts])
updateProject_secondaryArtifacts = Lens.lens (\UpdateProject' {secondaryArtifacts} -> secondaryArtifacts) (\s@UpdateProject' {} a -> s {secondaryArtifacts = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | Information to be changed about the build output artifacts for the build
-- project.
updateProject_artifacts :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectArtifacts)
updateProject_artifacts = Lens.lens (\UpdateProject' {artifacts} -> artifacts) (\s@UpdateProject' {} a -> s {artifacts = a} :: UpdateProject)

-- | Information to be changed about the build environment for the build
-- project.
updateProject_environment :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectEnvironment)
updateProject_environment = Lens.lens (\UpdateProject' {environment} -> environment) (\s@UpdateProject' {} a -> s {environment = a} :: UpdateProject)

-- | The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- To remove this limit, set this value to -1.
updateProject_concurrentBuildLimit :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Int)
updateProject_concurrentBuildLimit = Lens.lens (\UpdateProject' {concurrentBuildLimit} -> concurrentBuildLimit) (\s@UpdateProject' {} a -> s {concurrentBuildLimit = a} :: UpdateProject)

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
updateProject_badgeEnabled :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Bool)
updateProject_badgeEnabled = Lens.lens (\UpdateProject' {badgeEnabled} -> badgeEnabled) (\s@UpdateProject' {} a -> s {badgeEnabled = a} :: UpdateProject)

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
updateProject_secondarySourceVersions :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectSourceVersion])
updateProject_secondarySourceVersions = Lens.lens (\UpdateProject' {secondarySourceVersions} -> secondarySourceVersions) (\s@UpdateProject' {} a -> s {secondarySourceVersions = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | The number of minutes a build is allowed to be queued before it times
-- out.
updateProject_queuedTimeoutInMinutes :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Natural)
updateProject_queuedTimeoutInMinutes = Lens.lens (\UpdateProject' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@UpdateProject' {} a -> s {queuedTimeoutInMinutes = a} :: UpdateProject)

-- | Stores recently used information so that it can be quickly accessed at a
-- later time.
updateProject_cache :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectCache)
updateProject_cache = Lens.lens (\UpdateProject' {cache} -> cache) (\s@UpdateProject' {} a -> s {cache = a} :: UpdateProject)

-- | An array of @ProjectSource@ objects.
updateProject_secondarySources :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectSource])
updateProject_secondarySources = Lens.lens (\UpdateProject' {secondarySources} -> secondarySources) (\s@UpdateProject' {} a -> s {secondarySources = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
updateProject_sourceVersion :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_sourceVersion = Lens.lens (\UpdateProject' {sourceVersion} -> sourceVersion) (\s@UpdateProject' {} a -> s {sourceVersion = a} :: UpdateProject)

-- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
updateProject_vpcConfig :: Lens.Lens' UpdateProject (Prelude.Maybe VpcConfig)
updateProject_vpcConfig = Lens.lens (\UpdateProject' {vpcConfig} -> vpcConfig) (\s@UpdateProject' {} a -> s {vpcConfig = a} :: UpdateProject)

-- | Information to be changed about the build input source code for the
-- build project.
updateProject_source :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectSource)
updateProject_source = Lens.lens (\UpdateProject' {source} -> source) (\s@UpdateProject' {} a -> s {source = a} :: UpdateProject)

-- | Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, logs in an S3 bucket, or both.
updateProject_logsConfig :: Lens.Lens' UpdateProject (Prelude.Maybe LogsConfig)
updateProject_logsConfig = Lens.lens (\UpdateProject' {logsConfig} -> logsConfig) (\s@UpdateProject' {} a -> s {logsConfig = a} :: UpdateProject)

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
updateProject_fileSystemLocations :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectFileSystemLocation])
updateProject_fileSystemLocations = Lens.lens (\UpdateProject' {fileSystemLocations} -> fileSystemLocations) (\s@UpdateProject' {} a -> s {fileSystemLocations = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateProject_buildBatchConfig :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectBuildBatchConfig)
updateProject_buildBatchConfig = Lens.lens (\UpdateProject' {buildBatchConfig} -> buildBatchConfig) (\s@UpdateProject' {} a -> s {buildBatchConfig = a} :: UpdateProject)

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
updateProject_encryptionKey :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_encryptionKey = Lens.lens (\UpdateProject' {encryptionKey} -> encryptionKey) (\s@UpdateProject' {} a -> s {encryptionKey = a} :: UpdateProject)

-- | A new or replacement description of the build project.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject)

-- | The replacement ARN of the IAM role that enables CodeBuild to interact
-- with dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
updateProject_serviceRole :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_serviceRole = Lens.lens (\UpdateProject' {serviceRole} -> serviceRole) (\s@UpdateProject' {} a -> s {serviceRole = a} :: UpdateProject)

-- | An updated list of tag key and value pairs associated with this build
-- project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
updateProject_tags :: Lens.Lens' UpdateProject (Prelude.Maybe [Tag])
updateProject_tags = Lens.lens (\UpdateProject' {tags} -> tags) (\s@UpdateProject' {} a -> s {tags = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
-- to wait before timing out any related build that did not get marked as
-- completed.
updateProject_timeoutInMinutes :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Natural)
updateProject_timeoutInMinutes = Lens.lens (\UpdateProject' {timeoutInMinutes} -> timeoutInMinutes) (\s@UpdateProject' {} a -> s {timeoutInMinutes = a} :: UpdateProject)

-- | The name of the build project.
--
-- You cannot change a build project\'s name.
updateProject_name :: Lens.Lens' UpdateProject Prelude.Text
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (x Core..?> "project")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt salt' UpdateProject' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` buildBatchConfig
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` badgeEnabled
      `Prelude.hashWithSalt` concurrentBuildLimit
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` secondaryArtifacts

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf buildBatchConfig
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf secondarySources
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf secondarySourceVersions
      `Prelude.seq` Prelude.rnf badgeEnabled
      `Prelude.seq` Prelude.rnf concurrentBuildLimit
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf artifacts

instance Core.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.UpdateProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("secondaryArtifacts" Core..=)
              Prelude.<$> secondaryArtifacts,
            ("artifacts" Core..=) Prelude.<$> artifacts,
            ("environment" Core..=) Prelude.<$> environment,
            ("concurrentBuildLimit" Core..=)
              Prelude.<$> concurrentBuildLimit,
            ("badgeEnabled" Core..=) Prelude.<$> badgeEnabled,
            ("secondarySourceVersions" Core..=)
              Prelude.<$> secondarySourceVersions,
            ("queuedTimeoutInMinutes" Core..=)
              Prelude.<$> queuedTimeoutInMinutes,
            ("cache" Core..=) Prelude.<$> cache,
            ("secondarySources" Core..=)
              Prelude.<$> secondarySources,
            ("sourceVersion" Core..=) Prelude.<$> sourceVersion,
            ("vpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("source" Core..=) Prelude.<$> source,
            ("logsConfig" Core..=) Prelude.<$> logsConfig,
            ("fileSystemLocations" Core..=)
              Prelude.<$> fileSystemLocations,
            ("buildBatchConfig" Core..=)
              Prelude.<$> buildBatchConfig,
            ("encryptionKey" Core..=) Prelude.<$> encryptionKey,
            ("description" Core..=) Prelude.<$> description,
            ("serviceRole" Core..=) Prelude.<$> serviceRole,
            ("tags" Core..=) Prelude.<$> tags,
            ("timeoutInMinutes" Core..=)
              Prelude.<$> timeoutInMinutes,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath UpdateProject where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Information about the build project that was changed.
    project :: Prelude.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'updateProjectResponse_project' - Information about the build project that was changed.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse'
    { project = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build project that was changed.
updateProjectResponse_project :: Lens.Lens' UpdateProjectResponse (Prelude.Maybe Project)
updateProjectResponse_project = Lens.lens (\UpdateProjectResponse' {project} -> project) (\s@UpdateProjectResponse' {} a -> s {project = a} :: UpdateProjectResponse)

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf project
      `Prelude.seq` Prelude.rnf httpStatus
