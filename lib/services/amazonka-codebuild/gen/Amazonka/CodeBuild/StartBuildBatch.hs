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
-- Module      : Amazonka.CodeBuild.StartBuildBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a batch build for a project.
module Amazonka.CodeBuild.StartBuildBatch
  ( -- * Creating a Request
    StartBuildBatch (..),
    newStartBuildBatch,

    -- * Request Lenses
    startBuildBatch_encryptionKeyOverride,
    startBuildBatch_sourceLocationOverride,
    startBuildBatch_buildBatchConfigOverride,
    startBuildBatch_environmentVariablesOverride,
    startBuildBatch_idempotencyToken,
    startBuildBatch_debugSessionEnabled,
    startBuildBatch_registryCredentialOverride,
    startBuildBatch_serviceRoleOverride,
    startBuildBatch_cacheOverride,
    startBuildBatch_queuedTimeoutInMinutesOverride,
    startBuildBatch_secondarySourcesOverride,
    startBuildBatch_gitCloneDepthOverride,
    startBuildBatch_imagePullCredentialsTypeOverride,
    startBuildBatch_logsConfigOverride,
    startBuildBatch_sourceAuthOverride,
    startBuildBatch_gitSubmodulesConfigOverride,
    startBuildBatch_environmentTypeOverride,
    startBuildBatch_certificateOverride,
    startBuildBatch_computeTypeOverride,
    startBuildBatch_reportBuildBatchStatusOverride,
    startBuildBatch_privilegedModeOverride,
    startBuildBatch_sourceVersion,
    startBuildBatch_buildspecOverride,
    startBuildBatch_secondarySourcesVersionOverride,
    startBuildBatch_insecureSslOverride,
    startBuildBatch_imageOverride,
    startBuildBatch_secondaryArtifactsOverride,
    startBuildBatch_buildTimeoutInMinutesOverride,
    startBuildBatch_artifactsOverride,
    startBuildBatch_sourceTypeOverride,
    startBuildBatch_projectName,

    -- * Destructuring the Response
    StartBuildBatchResponse (..),
    newStartBuildBatchResponse,

    -- * Response Lenses
    startBuildBatchResponse_buildBatch,
    startBuildBatchResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBuildBatch' smart constructor.
data StartBuildBatch = StartBuildBatch'
  { -- | The Key Management Service customer master key (CMK) that overrides the
    -- one specified in the batch build project. The CMK key encrypts the build
    -- output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKeyOverride :: Prelude.Maybe Prelude.Text,
    -- | A location that overrides, for this batch build, the source location
    -- defined in the batch build project.
    sourceLocationOverride :: Prelude.Maybe Prelude.Text,
    -- | A @BuildBatchConfigOverride@ object that contains batch build
    -- configuration overrides.
    buildBatchConfigOverride :: Prelude.Maybe ProjectBuildBatchConfig,
    -- | An array of @EnvironmentVariable@ objects that override, or add to, the
    -- environment variables defined in the batch build project.
    environmentVariablesOverride :: Prelude.Maybe [EnvironmentVariable],
    -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the @StartBuildBatch@ request. The token is included in
    -- the @StartBuildBatch@ request and is valid for five minutes. If you
    -- repeat the @StartBuildBatch@ request with the same token, but change a
    -- parameter, CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies if session debugging is enabled for this batch build. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
    -- Batch session debugging is not supported for matrix batch builds.
    debugSessionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A @RegistryCredential@ object that overrides credentials for access to a
    -- private registry.
    registryCredentialOverride :: Prelude.Maybe RegistryCredential,
    -- | The name of a service role for this batch build that overrides the one
    -- specified in the batch build project.
    serviceRoleOverride :: Prelude.Maybe Prelude.Text,
    -- | A @ProjectCache@ object that specifies cache overrides.
    cacheOverride :: Prelude.Maybe ProjectCache,
    -- | The number of minutes a batch build is allowed to be queued before it
    -- times out.
    queuedTimeoutInMinutesOverride :: Prelude.Maybe Prelude.Natural,
    -- | An array of @ProjectSource@ objects that override the secondary sources
    -- defined in the batch build project.
    secondarySourcesOverride :: Prelude.Maybe [ProjectSource],
    -- | The user-defined depth of history, with a minimum value of 0, that
    -- overrides, for this batch build only, any previous depth of history
    -- defined in the batch build project.
    gitCloneDepthOverride :: Prelude.Maybe Prelude.Natural,
    -- | The type of credentials CodeBuild uses to pull images in your batch
    -- build. There are two valid values:
    --
    -- [CODEBUILD]
    --     Specifies that CodeBuild uses its own credentials. This requires
    --     that you modify your ECR repository policy to trust CodeBuild\'s
    --     service principal.
    --
    -- [SERVICE_ROLE]
    --     Specifies that CodeBuild uses your build project\'s service role.
    --
    -- When using a cross-account or private registry image, you must use
    -- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
    -- must use @CODEBUILD@ credentials.
    imagePullCredentialsTypeOverride :: Prelude.Maybe ImagePullCredentialsType,
    -- | A @LogsConfig@ object that override the log settings defined in the
    -- batch build project.
    logsConfigOverride :: Prelude.Maybe LogsConfig,
    -- | A @SourceAuth@ object that overrides the one defined in the batch build
    -- project. This override applies only if the build project\'s source is
    -- BitBucket or GitHub.
    sourceAuthOverride :: Prelude.Maybe SourceAuth,
    -- | A @GitSubmodulesConfig@ object that overrides the Git submodules
    -- configuration for this batch build.
    gitSubmodulesConfigOverride :: Prelude.Maybe GitSubmodulesConfig,
    -- | A container type for this batch build that overrides the one specified
    -- in the batch build project.
    environmentTypeOverride :: Prelude.Maybe EnvironmentType,
    -- | The name of a certificate for this batch build that overrides the one
    -- specified in the batch build project.
    certificateOverride :: Prelude.Maybe Prelude.Text,
    -- | The name of a compute type for this batch build that overrides the one
    -- specified in the batch build project.
    computeTypeOverride :: Prelude.Maybe ComputeType,
    -- | Set to @true@ to report to your source provider the status of a batch
    -- build\'s start and completion. If you use this option with a source
    -- provider other than GitHub, GitHub Enterprise, or Bitbucket, an
    -- @invalidInputException@ is thrown.
    --
    -- The status of a build triggered by a webhook is always reported to your
    -- source provider.
    reportBuildBatchStatusOverride :: Prelude.Maybe Prelude.Bool,
    -- | Enable this flag to override privileged mode in the batch build project.
    privilegedModeOverride :: Prelude.Maybe Prelude.Bool,
    -- | The version of the batch build input to be built, for this build only.
    -- If not specified, the latest version is used. If specified, the contents
    -- depends on the source provider:
    --
    -- [CodeCommit]
    --     The commit ID, branch, or Git tag to use.
    --
    -- [GitHub]
    --     The commit ID, pull request ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- [Bitbucket]
    --     The commit ID, branch name, or tag name that corresponds to the
    --     version of the source code you want to build. If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- [Amazon S3]
    --     The version ID of the object that represents the build input ZIP
    --     file to use.
    --
    -- If @sourceVersion@ is specified at the project level, then this
    -- @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | A buildspec file declaration that overrides, for this build only, the
    -- latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition,
    -- the path to an alternate buildspec file relative to the value of the
    -- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
    -- bucket. The bucket must be in the same Amazon Web Services Region as the
    -- build project. Specify the buildspec file using its ARN (for example,
    -- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
    -- not provided or is set to an empty string, the source code must contain
    -- a buildspec file in its root directory. For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
    buildspecOverride :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectSourceVersion@ objects that override the secondary
    -- source versions in the batch build project.
    secondarySourcesVersionOverride :: Prelude.Maybe [ProjectSourceVersion],
    -- | Enable this flag to override the insecure SSL setting that is specified
    -- in the batch build project. The insecure SSL setting determines whether
    -- to ignore SSL warnings while connecting to the project source code. This
    -- override applies only if the build\'s source is GitHub Enterprise.
    insecureSslOverride :: Prelude.Maybe Prelude.Bool,
    -- | The name of an image for this batch build that overrides the one
    -- specified in the batch build project.
    imageOverride :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectArtifacts@ objects that override the secondary
    -- artifacts defined in the batch build project.
    secondaryArtifactsOverride :: Prelude.Maybe [ProjectArtifacts],
    -- | Overrides the build timeout specified in the batch build project.
    buildTimeoutInMinutesOverride :: Prelude.Maybe Prelude.Natural,
    -- | An array of @ProjectArtifacts@ objects that contains information about
    -- the build output artifact overrides for the build project.
    artifactsOverride :: Prelude.Maybe ProjectArtifacts,
    -- | The source input type that overrides the source input defined in the
    -- batch build project.
    sourceTypeOverride :: Prelude.Maybe SourceType,
    -- | The name of the project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKeyOverride', 'startBuildBatch_encryptionKeyOverride' - The Key Management Service customer master key (CMK) that overrides the
-- one specified in the batch build project. The CMK key encrypts the build
-- output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'sourceLocationOverride', 'startBuildBatch_sourceLocationOverride' - A location that overrides, for this batch build, the source location
-- defined in the batch build project.
--
-- 'buildBatchConfigOverride', 'startBuildBatch_buildBatchConfigOverride' - A @BuildBatchConfigOverride@ object that contains batch build
-- configuration overrides.
--
-- 'environmentVariablesOverride', 'startBuildBatch_environmentVariablesOverride' - An array of @EnvironmentVariable@ objects that override, or add to, the
-- environment variables defined in the batch build project.
--
-- 'idempotencyToken', 'startBuildBatch_idempotencyToken' - A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @StartBuildBatch@ request. The token is included in
-- the @StartBuildBatch@ request and is valid for five minutes. If you
-- repeat the @StartBuildBatch@ request with the same token, but change a
-- parameter, CodeBuild returns a parameter mismatch error.
--
-- 'debugSessionEnabled', 'startBuildBatch_debugSessionEnabled' - Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
--
-- 'registryCredentialOverride', 'startBuildBatch_registryCredentialOverride' - A @RegistryCredential@ object that overrides credentials for access to a
-- private registry.
--
-- 'serviceRoleOverride', 'startBuildBatch_serviceRoleOverride' - The name of a service role for this batch build that overrides the one
-- specified in the batch build project.
--
-- 'cacheOverride', 'startBuildBatch_cacheOverride' - A @ProjectCache@ object that specifies cache overrides.
--
-- 'queuedTimeoutInMinutesOverride', 'startBuildBatch_queuedTimeoutInMinutesOverride' - The number of minutes a batch build is allowed to be queued before it
-- times out.
--
-- 'secondarySourcesOverride', 'startBuildBatch_secondarySourcesOverride' - An array of @ProjectSource@ objects that override the secondary sources
-- defined in the batch build project.
--
-- 'gitCloneDepthOverride', 'startBuildBatch_gitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that
-- overrides, for this batch build only, any previous depth of history
-- defined in the batch build project.
--
-- 'imagePullCredentialsTypeOverride', 'startBuildBatch_imagePullCredentialsTypeOverride' - The type of credentials CodeBuild uses to pull images in your batch
-- build. There are two valid values:
--
-- [CODEBUILD]
--     Specifies that CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that CodeBuild uses your build project\'s service role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
--
-- 'logsConfigOverride', 'startBuildBatch_logsConfigOverride' - A @LogsConfig@ object that override the log settings defined in the
-- batch build project.
--
-- 'sourceAuthOverride', 'startBuildBatch_sourceAuthOverride' - A @SourceAuth@ object that overrides the one defined in the batch build
-- project. This override applies only if the build project\'s source is
-- BitBucket or GitHub.
--
-- 'gitSubmodulesConfigOverride', 'startBuildBatch_gitSubmodulesConfigOverride' - A @GitSubmodulesConfig@ object that overrides the Git submodules
-- configuration for this batch build.
--
-- 'environmentTypeOverride', 'startBuildBatch_environmentTypeOverride' - A container type for this batch build that overrides the one specified
-- in the batch build project.
--
-- 'certificateOverride', 'startBuildBatch_certificateOverride' - The name of a certificate for this batch build that overrides the one
-- specified in the batch build project.
--
-- 'computeTypeOverride', 'startBuildBatch_computeTypeOverride' - The name of a compute type for this batch build that overrides the one
-- specified in the batch build project.
--
-- 'reportBuildBatchStatusOverride', 'startBuildBatch_reportBuildBatchStatusOverride' - Set to @true@ to report to your source provider the status of a batch
-- build\'s start and completion. If you use this option with a source
-- provider other than GitHub, GitHub Enterprise, or Bitbucket, an
-- @invalidInputException@ is thrown.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
--
-- 'privilegedModeOverride', 'startBuildBatch_privilegedModeOverride' - Enable this flag to override privileged mode in the batch build project.
--
-- 'sourceVersion', 'startBuildBatch_sourceVersion' - The version of the batch build input to be built, for this build only.
-- If not specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [CodeCommit]
--     The commit ID, branch, or Git tag to use.
--
-- [GitHub]
--     The commit ID, pull request ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Bitbucket]
--     The commit ID, branch name, or tag name that corresponds to the
--     version of the source code you want to build. If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Amazon S3]
--     The version ID of the object that represents the build input ZIP
--     file to use.
--
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'buildspecOverride', 'startBuildBatch_buildspecOverride' - A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same Amazon Web Services Region as the
-- build project. Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
--
-- 'secondarySourcesVersionOverride', 'startBuildBatch_secondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that override the secondary
-- source versions in the batch build project.
--
-- 'insecureSslOverride', 'startBuildBatch_insecureSslOverride' - Enable this flag to override the insecure SSL setting that is specified
-- in the batch build project. The insecure SSL setting determines whether
-- to ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
--
-- 'imageOverride', 'startBuildBatch_imageOverride' - The name of an image for this batch build that overrides the one
-- specified in the batch build project.
--
-- 'secondaryArtifactsOverride', 'startBuildBatch_secondaryArtifactsOverride' - An array of @ProjectArtifacts@ objects that override the secondary
-- artifacts defined in the batch build project.
--
-- 'buildTimeoutInMinutesOverride', 'startBuildBatch_buildTimeoutInMinutesOverride' - Overrides the build timeout specified in the batch build project.
--
-- 'artifactsOverride', 'startBuildBatch_artifactsOverride' - An array of @ProjectArtifacts@ objects that contains information about
-- the build output artifact overrides for the build project.
--
-- 'sourceTypeOverride', 'startBuildBatch_sourceTypeOverride' - The source input type that overrides the source input defined in the
-- batch build project.
--
-- 'projectName', 'startBuildBatch_projectName' - The name of the project.
newStartBuildBatch ::
  -- | 'projectName'
  Prelude.Text ->
  StartBuildBatch
newStartBuildBatch pProjectName_ =
  StartBuildBatch'
    { encryptionKeyOverride =
        Prelude.Nothing,
      sourceLocationOverride = Prelude.Nothing,
      buildBatchConfigOverride = Prelude.Nothing,
      environmentVariablesOverride = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      debugSessionEnabled = Prelude.Nothing,
      registryCredentialOverride = Prelude.Nothing,
      serviceRoleOverride = Prelude.Nothing,
      cacheOverride = Prelude.Nothing,
      queuedTimeoutInMinutesOverride = Prelude.Nothing,
      secondarySourcesOverride = Prelude.Nothing,
      gitCloneDepthOverride = Prelude.Nothing,
      imagePullCredentialsTypeOverride = Prelude.Nothing,
      logsConfigOverride = Prelude.Nothing,
      sourceAuthOverride = Prelude.Nothing,
      gitSubmodulesConfigOverride = Prelude.Nothing,
      environmentTypeOverride = Prelude.Nothing,
      certificateOverride = Prelude.Nothing,
      computeTypeOverride = Prelude.Nothing,
      reportBuildBatchStatusOverride = Prelude.Nothing,
      privilegedModeOverride = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      buildspecOverride = Prelude.Nothing,
      secondarySourcesVersionOverride = Prelude.Nothing,
      insecureSslOverride = Prelude.Nothing,
      imageOverride = Prelude.Nothing,
      secondaryArtifactsOverride = Prelude.Nothing,
      buildTimeoutInMinutesOverride = Prelude.Nothing,
      artifactsOverride = Prelude.Nothing,
      sourceTypeOverride = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | The Key Management Service customer master key (CMK) that overrides the
-- one specified in the batch build project. The CMK key encrypts the build
-- output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
startBuildBatch_encryptionKeyOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_encryptionKeyOverride = Lens.lens (\StartBuildBatch' {encryptionKeyOverride} -> encryptionKeyOverride) (\s@StartBuildBatch' {} a -> s {encryptionKeyOverride = a} :: StartBuildBatch)

-- | A location that overrides, for this batch build, the source location
-- defined in the batch build project.
startBuildBatch_sourceLocationOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_sourceLocationOverride = Lens.lens (\StartBuildBatch' {sourceLocationOverride} -> sourceLocationOverride) (\s@StartBuildBatch' {} a -> s {sourceLocationOverride = a} :: StartBuildBatch)

-- | A @BuildBatchConfigOverride@ object that contains batch build
-- configuration overrides.
startBuildBatch_buildBatchConfigOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe ProjectBuildBatchConfig)
startBuildBatch_buildBatchConfigOverride = Lens.lens (\StartBuildBatch' {buildBatchConfigOverride} -> buildBatchConfigOverride) (\s@StartBuildBatch' {} a -> s {buildBatchConfigOverride = a} :: StartBuildBatch)

-- | An array of @EnvironmentVariable@ objects that override, or add to, the
-- environment variables defined in the batch build project.
startBuildBatch_environmentVariablesOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe [EnvironmentVariable])
startBuildBatch_environmentVariablesOverride = Lens.lens (\StartBuildBatch' {environmentVariablesOverride} -> environmentVariablesOverride) (\s@StartBuildBatch' {} a -> s {environmentVariablesOverride = a} :: StartBuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @StartBuildBatch@ request. The token is included in
-- the @StartBuildBatch@ request and is valid for five minutes. If you
-- repeat the @StartBuildBatch@ request with the same token, but change a
-- parameter, CodeBuild returns a parameter mismatch error.
startBuildBatch_idempotencyToken :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_idempotencyToken = Lens.lens (\StartBuildBatch' {idempotencyToken} -> idempotencyToken) (\s@StartBuildBatch' {} a -> s {idempotencyToken = a} :: StartBuildBatch)

-- | Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
startBuildBatch_debugSessionEnabled :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Bool)
startBuildBatch_debugSessionEnabled = Lens.lens (\StartBuildBatch' {debugSessionEnabled} -> debugSessionEnabled) (\s@StartBuildBatch' {} a -> s {debugSessionEnabled = a} :: StartBuildBatch)

-- | A @RegistryCredential@ object that overrides credentials for access to a
-- private registry.
startBuildBatch_registryCredentialOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe RegistryCredential)
startBuildBatch_registryCredentialOverride = Lens.lens (\StartBuildBatch' {registryCredentialOverride} -> registryCredentialOverride) (\s@StartBuildBatch' {} a -> s {registryCredentialOverride = a} :: StartBuildBatch)

-- | The name of a service role for this batch build that overrides the one
-- specified in the batch build project.
startBuildBatch_serviceRoleOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_serviceRoleOverride = Lens.lens (\StartBuildBatch' {serviceRoleOverride} -> serviceRoleOverride) (\s@StartBuildBatch' {} a -> s {serviceRoleOverride = a} :: StartBuildBatch)

-- | A @ProjectCache@ object that specifies cache overrides.
startBuildBatch_cacheOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe ProjectCache)
startBuildBatch_cacheOverride = Lens.lens (\StartBuildBatch' {cacheOverride} -> cacheOverride) (\s@StartBuildBatch' {} a -> s {cacheOverride = a} :: StartBuildBatch)

-- | The number of minutes a batch build is allowed to be queued before it
-- times out.
startBuildBatch_queuedTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Natural)
startBuildBatch_queuedTimeoutInMinutesOverride = Lens.lens (\StartBuildBatch' {queuedTimeoutInMinutesOverride} -> queuedTimeoutInMinutesOverride) (\s@StartBuildBatch' {} a -> s {queuedTimeoutInMinutesOverride = a} :: StartBuildBatch)

-- | An array of @ProjectSource@ objects that override the secondary sources
-- defined in the batch build project.
startBuildBatch_secondarySourcesOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe [ProjectSource])
startBuildBatch_secondarySourcesOverride = Lens.lens (\StartBuildBatch' {secondarySourcesOverride} -> secondarySourcesOverride) (\s@StartBuildBatch' {} a -> s {secondarySourcesOverride = a} :: StartBuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | The user-defined depth of history, with a minimum value of 0, that
-- overrides, for this batch build only, any previous depth of history
-- defined in the batch build project.
startBuildBatch_gitCloneDepthOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Natural)
startBuildBatch_gitCloneDepthOverride = Lens.lens (\StartBuildBatch' {gitCloneDepthOverride} -> gitCloneDepthOverride) (\s@StartBuildBatch' {} a -> s {gitCloneDepthOverride = a} :: StartBuildBatch)

-- | The type of credentials CodeBuild uses to pull images in your batch
-- build. There are two valid values:
--
-- [CODEBUILD]
--     Specifies that CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that CodeBuild uses your build project\'s service role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
startBuildBatch_imagePullCredentialsTypeOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe ImagePullCredentialsType)
startBuildBatch_imagePullCredentialsTypeOverride = Lens.lens (\StartBuildBatch' {imagePullCredentialsTypeOverride} -> imagePullCredentialsTypeOverride) (\s@StartBuildBatch' {} a -> s {imagePullCredentialsTypeOverride = a} :: StartBuildBatch)

-- | A @LogsConfig@ object that override the log settings defined in the
-- batch build project.
startBuildBatch_logsConfigOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe LogsConfig)
startBuildBatch_logsConfigOverride = Lens.lens (\StartBuildBatch' {logsConfigOverride} -> logsConfigOverride) (\s@StartBuildBatch' {} a -> s {logsConfigOverride = a} :: StartBuildBatch)

-- | A @SourceAuth@ object that overrides the one defined in the batch build
-- project. This override applies only if the build project\'s source is
-- BitBucket or GitHub.
startBuildBatch_sourceAuthOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe SourceAuth)
startBuildBatch_sourceAuthOverride = Lens.lens (\StartBuildBatch' {sourceAuthOverride} -> sourceAuthOverride) (\s@StartBuildBatch' {} a -> s {sourceAuthOverride = a} :: StartBuildBatch)

-- | A @GitSubmodulesConfig@ object that overrides the Git submodules
-- configuration for this batch build.
startBuildBatch_gitSubmodulesConfigOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe GitSubmodulesConfig)
startBuildBatch_gitSubmodulesConfigOverride = Lens.lens (\StartBuildBatch' {gitSubmodulesConfigOverride} -> gitSubmodulesConfigOverride) (\s@StartBuildBatch' {} a -> s {gitSubmodulesConfigOverride = a} :: StartBuildBatch)

-- | A container type for this batch build that overrides the one specified
-- in the batch build project.
startBuildBatch_environmentTypeOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe EnvironmentType)
startBuildBatch_environmentTypeOverride = Lens.lens (\StartBuildBatch' {environmentTypeOverride} -> environmentTypeOverride) (\s@StartBuildBatch' {} a -> s {environmentTypeOverride = a} :: StartBuildBatch)

-- | The name of a certificate for this batch build that overrides the one
-- specified in the batch build project.
startBuildBatch_certificateOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_certificateOverride = Lens.lens (\StartBuildBatch' {certificateOverride} -> certificateOverride) (\s@StartBuildBatch' {} a -> s {certificateOverride = a} :: StartBuildBatch)

-- | The name of a compute type for this batch build that overrides the one
-- specified in the batch build project.
startBuildBatch_computeTypeOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe ComputeType)
startBuildBatch_computeTypeOverride = Lens.lens (\StartBuildBatch' {computeTypeOverride} -> computeTypeOverride) (\s@StartBuildBatch' {} a -> s {computeTypeOverride = a} :: StartBuildBatch)

-- | Set to @true@ to report to your source provider the status of a batch
-- build\'s start and completion. If you use this option with a source
-- provider other than GitHub, GitHub Enterprise, or Bitbucket, an
-- @invalidInputException@ is thrown.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
startBuildBatch_reportBuildBatchStatusOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Bool)
startBuildBatch_reportBuildBatchStatusOverride = Lens.lens (\StartBuildBatch' {reportBuildBatchStatusOverride} -> reportBuildBatchStatusOverride) (\s@StartBuildBatch' {} a -> s {reportBuildBatchStatusOverride = a} :: StartBuildBatch)

-- | Enable this flag to override privileged mode in the batch build project.
startBuildBatch_privilegedModeOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Bool)
startBuildBatch_privilegedModeOverride = Lens.lens (\StartBuildBatch' {privilegedModeOverride} -> privilegedModeOverride) (\s@StartBuildBatch' {} a -> s {privilegedModeOverride = a} :: StartBuildBatch)

-- | The version of the batch build input to be built, for this build only.
-- If not specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [CodeCommit]
--     The commit ID, branch, or Git tag to use.
--
-- [GitHub]
--     The commit ID, pull request ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Bitbucket]
--     The commit ID, branch name, or tag name that corresponds to the
--     version of the source code you want to build. If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Amazon S3]
--     The version ID of the object that represents the build input ZIP
--     file to use.
--
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
startBuildBatch_sourceVersion :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_sourceVersion = Lens.lens (\StartBuildBatch' {sourceVersion} -> sourceVersion) (\s@StartBuildBatch' {} a -> s {sourceVersion = a} :: StartBuildBatch)

-- | A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same Amazon Web Services Region as the
-- build project. Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
startBuildBatch_buildspecOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_buildspecOverride = Lens.lens (\StartBuildBatch' {buildspecOverride} -> buildspecOverride) (\s@StartBuildBatch' {} a -> s {buildspecOverride = a} :: StartBuildBatch)

-- | An array of @ProjectSourceVersion@ objects that override the secondary
-- source versions in the batch build project.
startBuildBatch_secondarySourcesVersionOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe [ProjectSourceVersion])
startBuildBatch_secondarySourcesVersionOverride = Lens.lens (\StartBuildBatch' {secondarySourcesVersionOverride} -> secondarySourcesVersionOverride) (\s@StartBuildBatch' {} a -> s {secondarySourcesVersionOverride = a} :: StartBuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | Enable this flag to override the insecure SSL setting that is specified
-- in the batch build project. The insecure SSL setting determines whether
-- to ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
startBuildBatch_insecureSslOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Bool)
startBuildBatch_insecureSslOverride = Lens.lens (\StartBuildBatch' {insecureSslOverride} -> insecureSslOverride) (\s@StartBuildBatch' {} a -> s {insecureSslOverride = a} :: StartBuildBatch)

-- | The name of an image for this batch build that overrides the one
-- specified in the batch build project.
startBuildBatch_imageOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Text)
startBuildBatch_imageOverride = Lens.lens (\StartBuildBatch' {imageOverride} -> imageOverride) (\s@StartBuildBatch' {} a -> s {imageOverride = a} :: StartBuildBatch)

-- | An array of @ProjectArtifacts@ objects that override the secondary
-- artifacts defined in the batch build project.
startBuildBatch_secondaryArtifactsOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe [ProjectArtifacts])
startBuildBatch_secondaryArtifactsOverride = Lens.lens (\StartBuildBatch' {secondaryArtifactsOverride} -> secondaryArtifactsOverride) (\s@StartBuildBatch' {} a -> s {secondaryArtifactsOverride = a} :: StartBuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | Overrides the build timeout specified in the batch build project.
startBuildBatch_buildTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe Prelude.Natural)
startBuildBatch_buildTimeoutInMinutesOverride = Lens.lens (\StartBuildBatch' {buildTimeoutInMinutesOverride} -> buildTimeoutInMinutesOverride) (\s@StartBuildBatch' {} a -> s {buildTimeoutInMinutesOverride = a} :: StartBuildBatch)

-- | An array of @ProjectArtifacts@ objects that contains information about
-- the build output artifact overrides for the build project.
startBuildBatch_artifactsOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe ProjectArtifacts)
startBuildBatch_artifactsOverride = Lens.lens (\StartBuildBatch' {artifactsOverride} -> artifactsOverride) (\s@StartBuildBatch' {} a -> s {artifactsOverride = a} :: StartBuildBatch)

-- | The source input type that overrides the source input defined in the
-- batch build project.
startBuildBatch_sourceTypeOverride :: Lens.Lens' StartBuildBatch (Prelude.Maybe SourceType)
startBuildBatch_sourceTypeOverride = Lens.lens (\StartBuildBatch' {sourceTypeOverride} -> sourceTypeOverride) (\s@StartBuildBatch' {} a -> s {sourceTypeOverride = a} :: StartBuildBatch)

-- | The name of the project.
startBuildBatch_projectName :: Lens.Lens' StartBuildBatch Prelude.Text
startBuildBatch_projectName = Lens.lens (\StartBuildBatch' {projectName} -> projectName) (\s@StartBuildBatch' {} a -> s {projectName = a} :: StartBuildBatch)

instance Core.AWSRequest StartBuildBatch where
  type
    AWSResponse StartBuildBatch =
      StartBuildBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBuildBatchResponse'
            Prelude.<$> (x Core..?> "buildBatch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBuildBatch where
  hashWithSalt salt' StartBuildBatch' {..} =
    salt' `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` sourceTypeOverride
      `Prelude.hashWithSalt` artifactsOverride
      `Prelude.hashWithSalt` buildTimeoutInMinutesOverride
      `Prelude.hashWithSalt` secondaryArtifactsOverride
      `Prelude.hashWithSalt` imageOverride
      `Prelude.hashWithSalt` insecureSslOverride
      `Prelude.hashWithSalt` secondarySourcesVersionOverride
      `Prelude.hashWithSalt` buildspecOverride
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` privilegedModeOverride
      `Prelude.hashWithSalt` reportBuildBatchStatusOverride
      `Prelude.hashWithSalt` computeTypeOverride
      `Prelude.hashWithSalt` certificateOverride
      `Prelude.hashWithSalt` environmentTypeOverride
      `Prelude.hashWithSalt` gitSubmodulesConfigOverride
      `Prelude.hashWithSalt` sourceAuthOverride
      `Prelude.hashWithSalt` logsConfigOverride
      `Prelude.hashWithSalt` imagePullCredentialsTypeOverride
      `Prelude.hashWithSalt` gitCloneDepthOverride
      `Prelude.hashWithSalt` secondarySourcesOverride
      `Prelude.hashWithSalt` queuedTimeoutInMinutesOverride
      `Prelude.hashWithSalt` cacheOverride
      `Prelude.hashWithSalt` serviceRoleOverride
      `Prelude.hashWithSalt` registryCredentialOverride
      `Prelude.hashWithSalt` debugSessionEnabled
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` environmentVariablesOverride
      `Prelude.hashWithSalt` buildBatchConfigOverride
      `Prelude.hashWithSalt` sourceLocationOverride
      `Prelude.hashWithSalt` encryptionKeyOverride

instance Prelude.NFData StartBuildBatch where
  rnf StartBuildBatch' {..} =
    Prelude.rnf encryptionKeyOverride
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf sourceTypeOverride
      `Prelude.seq` Prelude.rnf artifactsOverride
      `Prelude.seq` Prelude.rnf buildTimeoutInMinutesOverride
      `Prelude.seq` Prelude.rnf secondaryArtifactsOverride
      `Prelude.seq` Prelude.rnf imageOverride
      `Prelude.seq` Prelude.rnf insecureSslOverride
      `Prelude.seq` Prelude.rnf secondarySourcesVersionOverride
      `Prelude.seq` Prelude.rnf buildspecOverride
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf privilegedModeOverride
      `Prelude.seq` Prelude.rnf reportBuildBatchStatusOverride
      `Prelude.seq` Prelude.rnf computeTypeOverride
      `Prelude.seq` Prelude.rnf certificateOverride
      `Prelude.seq` Prelude.rnf environmentTypeOverride
      `Prelude.seq` Prelude.rnf gitSubmodulesConfigOverride
      `Prelude.seq` Prelude.rnf sourceAuthOverride
      `Prelude.seq` Prelude.rnf logsConfigOverride
      `Prelude.seq` Prelude.rnf imagePullCredentialsTypeOverride
      `Prelude.seq` Prelude.rnf gitCloneDepthOverride
      `Prelude.seq` Prelude.rnf secondarySourcesOverride
      `Prelude.seq` Prelude.rnf queuedTimeoutInMinutesOverride
      `Prelude.seq` Prelude.rnf cacheOverride
      `Prelude.seq` Prelude.rnf serviceRoleOverride
      `Prelude.seq` Prelude.rnf registryCredentialOverride
      `Prelude.seq` Prelude.rnf debugSessionEnabled
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf environmentVariablesOverride
      `Prelude.seq` Prelude.rnf buildBatchConfigOverride
      `Prelude.seq` Prelude.rnf sourceLocationOverride

instance Core.ToHeaders StartBuildBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.StartBuildBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartBuildBatch where
  toJSON StartBuildBatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionKeyOverride" Core..=)
              Prelude.<$> encryptionKeyOverride,
            ("sourceLocationOverride" Core..=)
              Prelude.<$> sourceLocationOverride,
            ("buildBatchConfigOverride" Core..=)
              Prelude.<$> buildBatchConfigOverride,
            ("environmentVariablesOverride" Core..=)
              Prelude.<$> environmentVariablesOverride,
            ("idempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("debugSessionEnabled" Core..=)
              Prelude.<$> debugSessionEnabled,
            ("registryCredentialOverride" Core..=)
              Prelude.<$> registryCredentialOverride,
            ("serviceRoleOverride" Core..=)
              Prelude.<$> serviceRoleOverride,
            ("cacheOverride" Core..=) Prelude.<$> cacheOverride,
            ("queuedTimeoutInMinutesOverride" Core..=)
              Prelude.<$> queuedTimeoutInMinutesOverride,
            ("secondarySourcesOverride" Core..=)
              Prelude.<$> secondarySourcesOverride,
            ("gitCloneDepthOverride" Core..=)
              Prelude.<$> gitCloneDepthOverride,
            ("imagePullCredentialsTypeOverride" Core..=)
              Prelude.<$> imagePullCredentialsTypeOverride,
            ("logsConfigOverride" Core..=)
              Prelude.<$> logsConfigOverride,
            ("sourceAuthOverride" Core..=)
              Prelude.<$> sourceAuthOverride,
            ("gitSubmodulesConfigOverride" Core..=)
              Prelude.<$> gitSubmodulesConfigOverride,
            ("environmentTypeOverride" Core..=)
              Prelude.<$> environmentTypeOverride,
            ("certificateOverride" Core..=)
              Prelude.<$> certificateOverride,
            ("computeTypeOverride" Core..=)
              Prelude.<$> computeTypeOverride,
            ("reportBuildBatchStatusOverride" Core..=)
              Prelude.<$> reportBuildBatchStatusOverride,
            ("privilegedModeOverride" Core..=)
              Prelude.<$> privilegedModeOverride,
            ("sourceVersion" Core..=) Prelude.<$> sourceVersion,
            ("buildspecOverride" Core..=)
              Prelude.<$> buildspecOverride,
            ("secondarySourcesVersionOverride" Core..=)
              Prelude.<$> secondarySourcesVersionOverride,
            ("insecureSslOverride" Core..=)
              Prelude.<$> insecureSslOverride,
            ("imageOverride" Core..=) Prelude.<$> imageOverride,
            ("secondaryArtifactsOverride" Core..=)
              Prelude.<$> secondaryArtifactsOverride,
            ("buildTimeoutInMinutesOverride" Core..=)
              Prelude.<$> buildTimeoutInMinutesOverride,
            ("artifactsOverride" Core..=)
              Prelude.<$> artifactsOverride,
            ("sourceTypeOverride" Core..=)
              Prelude.<$> sourceTypeOverride,
            Prelude.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath StartBuildBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery StartBuildBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBuildBatchResponse' smart constructor.
data StartBuildBatchResponse = StartBuildBatchResponse'
  { -- | A @BuildBatch@ object that contains information about the batch build.
    buildBatch :: Prelude.Maybe BuildBatch,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBuildBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildBatch', 'startBuildBatchResponse_buildBatch' - A @BuildBatch@ object that contains information about the batch build.
--
-- 'httpStatus', 'startBuildBatchResponse_httpStatus' - The response's http status code.
newStartBuildBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBuildBatchResponse
newStartBuildBatchResponse pHttpStatus_ =
  StartBuildBatchResponse'
    { buildBatch =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @BuildBatch@ object that contains information about the batch build.
startBuildBatchResponse_buildBatch :: Lens.Lens' StartBuildBatchResponse (Prelude.Maybe BuildBatch)
startBuildBatchResponse_buildBatch = Lens.lens (\StartBuildBatchResponse' {buildBatch} -> buildBatch) (\s@StartBuildBatchResponse' {} a -> s {buildBatch = a} :: StartBuildBatchResponse)

-- | The response's http status code.
startBuildBatchResponse_httpStatus :: Lens.Lens' StartBuildBatchResponse Prelude.Int
startBuildBatchResponse_httpStatus = Lens.lens (\StartBuildBatchResponse' {httpStatus} -> httpStatus) (\s@StartBuildBatchResponse' {} a -> s {httpStatus = a} :: StartBuildBatchResponse)

instance Prelude.NFData StartBuildBatchResponse where
  rnf StartBuildBatchResponse' {..} =
    Prelude.rnf buildBatch
      `Prelude.seq` Prelude.rnf httpStatus
