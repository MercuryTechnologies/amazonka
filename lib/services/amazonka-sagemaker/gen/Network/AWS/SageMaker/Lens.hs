{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_nameContains,
    listProjects_creationTimeAfter,
    listProjects_nextToken,
    listProjects_sortOrder,
    listProjects_creationTimeBefore,
    listProjects_maxResults,
    listProjects_sortBy,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaryList,

    -- ** DeleteProject
    deleteProject_projectName,

    -- ** GetModelPackageGroupPolicy
    getModelPackageGroupPolicy_modelPackageGroupName,
    getModelPackageGroupPolicyResponse_httpStatus,
    getModelPackageGroupPolicyResponse_resourcePolicy,

    -- ** CreateNotebookInstance
    createNotebookInstance_acceleratorTypes,
    createNotebookInstance_platformIdentifier,
    createNotebookInstance_securityGroupIds,
    createNotebookInstance_additionalCodeRepositories,
    createNotebookInstance_lifecycleConfigName,
    createNotebookInstance_subnetId,
    createNotebookInstance_defaultCodeRepository,
    createNotebookInstance_volumeSizeInGB,
    createNotebookInstance_kmsKeyId,
    createNotebookInstance_rootAccess,
    createNotebookInstance_directInternetAccess,
    createNotebookInstance_tags,
    createNotebookInstance_notebookInstanceName,
    createNotebookInstance_instanceType,
    createNotebookInstance_roleArn,
    createNotebookInstanceResponse_notebookInstanceArn,
    createNotebookInstanceResponse_httpStatus,

    -- ** UpdateModelPackage
    updateModelPackage_approvalDescription,
    updateModelPackage_modelPackageArn,
    updateModelPackage_modelApprovalStatus,
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,

    -- ** DeleteModelPackage
    deleteModelPackage_modelPackageName,

    -- ** DescribeMonitoringSchedule
    describeMonitoringSchedule_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_endpointName,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,

    -- ** ListTrialComponents
    listTrialComponents_createdAfter,
    listTrialComponents_sourceArn,
    listTrialComponents_experimentName,
    listTrialComponents_nextToken,
    listTrialComponents_sortOrder,
    listTrialComponents_trialName,
    listTrialComponents_maxResults,
    listTrialComponents_createdBefore,
    listTrialComponents_sortBy,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_httpStatus,

    -- ** DescribeEndpointConfig
    describeEndpointConfig_endpointConfigName,
    describeEndpointConfigResponse_asyncInferenceConfig,
    describeEndpointConfigResponse_kmsKeyId,
    describeEndpointConfigResponse_dataCaptureConfig,
    describeEndpointConfigResponse_httpStatus,
    describeEndpointConfigResponse_endpointConfigName,
    describeEndpointConfigResponse_endpointConfigArn,
    describeEndpointConfigResponse_productionVariants,
    describeEndpointConfigResponse_creationTime,

    -- ** CreateModelExplainabilityJobDefinition
    createModelExplainabilityJobDefinition_stoppingCondition,
    createModelExplainabilityJobDefinition_networkConfig,
    createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig,
    createModelExplainabilityJobDefinition_tags,
    createModelExplainabilityJobDefinition_jobDefinitionName,
    createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification,
    createModelExplainabilityJobDefinition_modelExplainabilityJobInput,
    createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig,
    createModelExplainabilityJobDefinition_jobResources,
    createModelExplainabilityJobDefinition_roleArn,
    createModelExplainabilityJobDefinitionResponse_httpStatus,
    createModelExplainabilityJobDefinitionResponse_jobDefinitionArn,

    -- ** DescribeApp
    describeApp_domainId,
    describeApp_userProfileName,
    describeApp_appType,
    describeApp_appName,
    describeAppResponse_creationTime,
    describeAppResponse_status,
    describeAppResponse_failureReason,
    describeAppResponse_resourceSpec,
    describeAppResponse_userProfileName,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_appName,
    describeAppResponse_appArn,
    describeAppResponse_domainId,
    describeAppResponse_appType,
    describeAppResponse_httpStatus,

    -- ** ListImageVersions
    listImageVersions_lastModifiedTimeBefore,
    listImageVersions_creationTimeAfter,
    listImageVersions_nextToken,
    listImageVersions_sortOrder,
    listImageVersions_lastModifiedTimeAfter,
    listImageVersions_creationTimeBefore,
    listImageVersions_maxResults,
    listImageVersions_sortBy,
    listImageVersions_imageName,
    listImageVersionsResponse_nextToken,
    listImageVersionsResponse_imageVersions,
    listImageVersionsResponse_httpStatus,

    -- ** DescribeAutoMLJob
    describeAutoMLJob_autoMLJobName,
    describeAutoMLJobResponse_generateCandidateDefinitionsOnly,
    describeAutoMLJobResponse_failureReason,
    describeAutoMLJobResponse_partialFailureReasons,
    describeAutoMLJobResponse_modelDeployResult,
    describeAutoMLJobResponse_problemType,
    describeAutoMLJobResponse_autoMLJobConfig,
    describeAutoMLJobResponse_autoMLJobObjective,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_resolvedAttributes,
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_bestCandidate,
    describeAutoMLJobResponse_modelDeployConfig,
    describeAutoMLJobResponse_httpStatus,
    describeAutoMLJobResponse_autoMLJobName,
    describeAutoMLJobResponse_autoMLJobArn,
    describeAutoMLJobResponse_inputDataConfig,
    describeAutoMLJobResponse_outputDataConfig,
    describeAutoMLJobResponse_roleArn,
    describeAutoMLJobResponse_creationTime,
    describeAutoMLJobResponse_lastModifiedTime,
    describeAutoMLJobResponse_autoMLJobStatus,
    describeAutoMLJobResponse_autoMLJobSecondaryStatus,

    -- ** StopProcessingJob
    stopProcessingJob_processingJobName,

    -- ** DeleteAction
    deleteAction_actionName,
    deleteActionResponse_actionArn,
    deleteActionResponse_httpStatus,

    -- ** UpdateAction
    updateAction_status,
    updateAction_propertiesToRemove,
    updateAction_description,
    updateAction_properties,
    updateAction_actionName,
    updateActionResponse_actionArn,
    updateActionResponse_httpStatus,

    -- ** ListLabelingJobsForWorkteam
    listLabelingJobsForWorkteam_jobReferenceCodeContains,
    listLabelingJobsForWorkteam_creationTimeAfter,
    listLabelingJobsForWorkteam_nextToken,
    listLabelingJobsForWorkteam_sortOrder,
    listLabelingJobsForWorkteam_creationTimeBefore,
    listLabelingJobsForWorkteam_maxResults,
    listLabelingJobsForWorkteam_sortBy,
    listLabelingJobsForWorkteam_workteamArn,
    listLabelingJobsForWorkteamResponse_nextToken,
    listLabelingJobsForWorkteamResponse_httpStatus,
    listLabelingJobsForWorkteamResponse_labelingJobSummaryList,

    -- ** CreateTransformJob
    createTransformJob_modelClientConfig,
    createTransformJob_batchStrategy,
    createTransformJob_maxPayloadInMB,
    createTransformJob_environment,
    createTransformJob_experimentConfig,
    createTransformJob_maxConcurrentTransforms,
    createTransformJob_dataProcessing,
    createTransformJob_tags,
    createTransformJob_transformJobName,
    createTransformJob_modelName,
    createTransformJob_transformInput,
    createTransformJob_transformOutput,
    createTransformJob_transformResources,
    createTransformJobResponse_httpStatus,
    createTransformJobResponse_transformJobArn,

    -- ** ListArtifacts
    listArtifacts_createdAfter,
    listArtifacts_nextToken,
    listArtifacts_sortOrder,
    listArtifacts_sourceUri,
    listArtifacts_artifactType,
    listArtifacts_maxResults,
    listArtifacts_createdBefore,
    listArtifacts_sortBy,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,

    -- ** DeleteDeviceFleet
    deleteDeviceFleet_deviceFleetName,

    -- ** UpdateDeviceFleet
    updateDeviceFleet_enableIotRoleAlias,
    updateDeviceFleet_description,
    updateDeviceFleet_roleArn,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- ** ListCompilationJobs
    listCompilationJobs_nameContains,
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_creationTimeAfter,
    listCompilationJobs_nextToken,
    listCompilationJobs_sortOrder,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_statusEquals,
    listCompilationJobs_maxResults,
    listCompilationJobs_sortBy,
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,

    -- ** DescribePipeline
    describePipeline_pipelineName,
    describePipelineResponse_creationTime,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_pipelineName,
    describePipelineResponse_createdBy,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_roleArn,
    describePipelineResponse_httpStatus,

    -- ** DisassociateTrialComponent
    disassociateTrialComponent_trialComponentName,
    disassociateTrialComponent_trialName,
    disassociateTrialComponentResponse_trialArn,
    disassociateTrialComponentResponse_trialComponentArn,
    disassociateTrialComponentResponse_httpStatus,

    -- ** DescribeModelPackageGroup
    describeModelPackageGroup_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupDescription,
    describeModelPackageGroupResponse_httpStatus,
    describeModelPackageGroupResponse_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupArn,
    describeModelPackageGroupResponse_creationTime,
    describeModelPackageGroupResponse_createdBy,
    describeModelPackageGroupResponse_modelPackageGroupStatus,

    -- ** CreateEdgePackagingJob
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_tags,
    createEdgePackagingJob_edgePackagingJobName,
    createEdgePackagingJob_compilationJobName,
    createEdgePackagingJob_modelName,
    createEdgePackagingJob_modelVersion,
    createEdgePackagingJob_roleArn,
    createEdgePackagingJob_outputConfig,

    -- ** StopHyperParameterTuningJob
    stopHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** ListHumanTaskUis
    listHumanTaskUis_creationTimeAfter,
    listHumanTaskUis_nextToken,
    listHumanTaskUis_sortOrder,
    listHumanTaskUis_creationTimeBefore,
    listHumanTaskUis_maxResults,
    listHumanTaskUisResponse_nextToken,
    listHumanTaskUisResponse_httpStatus,
    listHumanTaskUisResponse_humanTaskUiSummaries,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_endpointConfigName,
    createEndpointResponse_httpStatus,
    createEndpointResponse_endpointArn,

    -- ** GetSearchSuggestions
    getSearchSuggestions_suggestionQuery,
    getSearchSuggestions_resource,
    getSearchSuggestionsResponse_propertyNameSuggestions,
    getSearchSuggestionsResponse_httpStatus,

    -- ** UpdateArtifact
    updateArtifact_propertiesToRemove,
    updateArtifact_artifactName,
    updateArtifact_properties,
    updateArtifact_artifactArn,
    updateArtifactResponse_artifactArn,
    updateArtifactResponse_httpStatus,

    -- ** DeleteArtifact
    deleteArtifact_source,
    deleteArtifact_artifactArn,
    deleteArtifactResponse_artifactArn,
    deleteArtifactResponse_httpStatus,

    -- ** DescribeTrial
    describeTrial_trialName,
    describeTrialResponse_creationTime,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_trialArn,
    describeTrialResponse_createdBy,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_experimentName,
    describeTrialResponse_source,
    describeTrialResponse_displayName,
    describeTrialResponse_trialName,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_httpStatus,

    -- ** ListActions
    listActions_createdAfter,
    listActions_nextToken,
    listActions_sortOrder,
    listActions_sourceUri,
    listActions_actionType,
    listActions_maxResults,
    listActions_createdBefore,
    listActions_sortBy,
    listActionsResponse_actionSummaries,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,

    -- ** CreateArtifact
    createArtifact_metadataProperties,
    createArtifact_artifactName,
    createArtifact_tags,
    createArtifact_properties,
    createArtifact_source,
    createArtifact_artifactType,
    createArtifactResponse_artifactArn,
    createArtifactResponse_httpStatus,

    -- ** CreatePresignedDomainUrl
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_expiresInSeconds,
    createPresignedDomainUrl_domainId,
    createPresignedDomainUrl_userProfileName,
    createPresignedDomainUrlResponse_authorizedUrl,
    createPresignedDomainUrlResponse_httpStatus,

    -- ** ListFeatureGroups
    listFeatureGroups_nameContains,
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_nextToken,
    listFeatureGroups_sortOrder,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroups_maxResults,
    listFeatureGroups_sortBy,
    listFeatureGroupsResponse_nextToken,
    listFeatureGroupsResponse_httpStatus,
    listFeatureGroupsResponse_featureGroupSummaries,

    -- ** DescribeCodeRepository
    describeCodeRepository_codeRepositoryName,
    describeCodeRepositoryResponse_gitConfig,
    describeCodeRepositoryResponse_httpStatus,
    describeCodeRepositoryResponse_codeRepositoryName,
    describeCodeRepositoryResponse_codeRepositoryArn,
    describeCodeRepositoryResponse_creationTime,
    describeCodeRepositoryResponse_lastModifiedTime,

    -- ** DescribeContext
    describeContext_contextName,
    describeContextResponse_creationTime,
    describeContextResponse_createdBy,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_contextType,
    describeContextResponse_contextArn,
    describeContextResponse_source,
    describeContextResponse_contextName,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_description,
    describeContextResponse_properties,
    describeContextResponse_httpStatus,

    -- ** DescribeImage
    describeImage_imageName,
    describeImageResponse_creationTime,
    describeImageResponse_failureReason,
    describeImageResponse_imageStatus,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_imageArn,
    describeImageResponse_displayName,
    describeImageResponse_imageName,
    describeImageResponse_description,
    describeImageResponse_roleArn,
    describeImageResponse_httpStatus,

    -- ** DescribeTrainingJob
    describeTrainingJob_trainingJobName,
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_failureReason,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_trainingEndTime,
    describeTrainingJobResponse_environment,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_retryStrategy,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_roleArn,
    describeTrainingJobResponse_httpStatus,
    describeTrainingJobResponse_trainingJobName,
    describeTrainingJobResponse_trainingJobArn,
    describeTrainingJobResponse_modelArtifacts,
    describeTrainingJobResponse_trainingJobStatus,
    describeTrainingJobResponse_secondaryStatus,
    describeTrainingJobResponse_algorithmSpecification,
    describeTrainingJobResponse_resourceConfig,
    describeTrainingJobResponse_stoppingCondition,
    describeTrainingJobResponse_creationTime,

    -- ** CreateAction
    createAction_metadataProperties,
    createAction_status,
    createAction_description,
    createAction_tags,
    createAction_properties,
    createAction_actionName,
    createAction_source,
    createAction_actionType,
    createActionResponse_actionArn,
    createActionResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointName,

    -- ** UpdateEndpoint
    updateEndpoint_excludeRetainedVariantProperties,
    updateEndpoint_retainAllVariantProperties,
    updateEndpoint_deploymentConfig,
    updateEndpoint_endpointName,
    updateEndpoint_endpointConfigName,
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_endpointArn,

    -- ** DescribeDataQualityJobDefinition
    describeDataQualityJobDefinition_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig,
    describeDataQualityJobDefinitionResponse_stoppingCondition,
    describeDataQualityJobDefinitionResponse_networkConfig,
    describeDataQualityJobDefinitionResponse_httpStatus,
    describeDataQualityJobDefinitionResponse_jobDefinitionArn,
    describeDataQualityJobDefinitionResponse_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_creationTime,
    describeDataQualityJobDefinitionResponse_dataQualityAppSpecification,
    describeDataQualityJobDefinitionResponse_dataQualityJobInput,
    describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig,
    describeDataQualityJobDefinitionResponse_jobResources,
    describeDataQualityJobDefinitionResponse_roleArn,

    -- ** CreateHumanTaskUi
    createHumanTaskUi_tags,
    createHumanTaskUi_humanTaskUiName,
    createHumanTaskUi_uiTemplate,
    createHumanTaskUiResponse_httpStatus,
    createHumanTaskUiResponse_humanTaskUiArn,

    -- ** RegisterDevices
    registerDevices_tags,
    registerDevices_deviceFleetName,
    registerDevices_devices,

    -- ** CreateCompilationJob
    createCompilationJob_vpcConfig,
    createCompilationJob_tags,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
    createCompilationJob_inputConfig,
    createCompilationJob_outputConfig,
    createCompilationJob_stoppingCondition,
    createCompilationJobResponse_httpStatus,
    createCompilationJobResponse_compilationJobArn,

    -- ** DeleteAppImageConfig
    deleteAppImageConfig_appImageConfigName,

    -- ** UpdateAppImageConfig
    updateAppImageConfig_kernelGatewayImageConfig,
    updateAppImageConfig_appImageConfigName,
    updateAppImageConfigResponse_appImageConfigArn,
    updateAppImageConfigResponse_httpStatus,

    -- ** DescribePipelineExecution
    describePipelineExecution_pipelineExecutionArn,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_failureReason,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_pipelineExperimentConfig,
    describePipelineExecutionResponse_httpStatus,

    -- ** DeleteNotebookInstanceLifecycleConfig
    deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- ** UpdateNotebookInstanceLifecycleConfig
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** DeleteWorkforce
    deleteWorkforce_workforceName,
    deleteWorkforceResponse_httpStatus,

    -- ** UpdateWorkforce
    updateWorkforce_sourceIpConfig,
    updateWorkforce_oidcConfig,
    updateWorkforce_workforceName,
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,

    -- ** ListProcessingJobs
    listProcessingJobs_nameContains,
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_creationTimeAfter,
    listProcessingJobs_nextToken,
    listProcessingJobs_sortOrder,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_statusEquals,
    listProcessingJobs_maxResults,
    listProcessingJobs_sortBy,
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,

    -- ** CreateLabelingJob
    createLabelingJob_labelingJobAlgorithmsConfig,
    createLabelingJob_labelCategoryConfigS3Uri,
    createLabelingJob_stoppingConditions,
    createLabelingJob_tags,
    createLabelingJob_labelingJobName,
    createLabelingJob_labelAttributeName,
    createLabelingJob_inputConfig,
    createLabelingJob_outputConfig,
    createLabelingJob_roleArn,
    createLabelingJob_humanTaskConfig,
    createLabelingJobResponse_httpStatus,
    createLabelingJobResponse_labelingJobArn,

    -- ** EnableSagemakerServicecatalogPortfolio
    enableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** DescribeNotebookInstance
    describeNotebookInstance_notebookInstanceName,
    describeNotebookInstanceResponse_creationTime,
    describeNotebookInstanceResponse_failureReason,
    describeNotebookInstanceResponse_acceleratorTypes,
    describeNotebookInstanceResponse_platformIdentifier,
    describeNotebookInstanceResponse_notebookInstanceName,
    describeNotebookInstanceResponse_securityGroups,
    describeNotebookInstanceResponse_additionalCodeRepositories,
    describeNotebookInstanceResponse_url,
    describeNotebookInstanceResponse_lastModifiedTime,
    describeNotebookInstanceResponse_networkInterfaceId,
    describeNotebookInstanceResponse_subnetId,
    describeNotebookInstanceResponse_instanceType,
    describeNotebookInstanceResponse_notebookInstanceStatus,
    describeNotebookInstanceResponse_defaultCodeRepository,
    describeNotebookInstanceResponse_volumeSizeInGB,
    describeNotebookInstanceResponse_kmsKeyId,
    describeNotebookInstanceResponse_rootAccess,
    describeNotebookInstanceResponse_directInternetAccess,
    describeNotebookInstanceResponse_notebookInstanceArn,
    describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceResponse_roleArn,
    describeNotebookInstanceResponse_httpStatus,

    -- ** CreateMonitoringSchedule
    createMonitoringSchedule_tags,
    createMonitoringSchedule_monitoringScheduleName,
    createMonitoringSchedule_monitoringScheduleConfig,
    createMonitoringScheduleResponse_httpStatus,
    createMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** ListAppImageConfigs
    listAppImageConfigs_nameContains,
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_sortOrder,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_sortBy,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_httpStatus,

    -- ** CreateEndpointConfig
    createEndpointConfig_asyncInferenceConfig,
    createEndpointConfig_kmsKeyId,
    createEndpointConfig_dataCaptureConfig,
    createEndpointConfig_tags,
    createEndpointConfig_endpointConfigName,
    createEndpointConfig_productionVariants,
    createEndpointConfigResponse_httpStatus,
    createEndpointConfigResponse_endpointConfigArn,

    -- ** SendPipelineExecutionStepSuccess
    sendPipelineExecutionStepSuccess_outputParameters,
    sendPipelineExecutionStepSuccess_clientRequestToken,
    sendPipelineExecutionStepSuccess_callbackToken,
    sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn,
    sendPipelineExecutionStepSuccessResponse_httpStatus,

    -- ** DescribeModelQualityJobDefinition
    describeModelQualityJobDefinition_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig,
    describeModelQualityJobDefinitionResponse_stoppingCondition,
    describeModelQualityJobDefinitionResponse_networkConfig,
    describeModelQualityJobDefinitionResponse_httpStatus,
    describeModelQualityJobDefinitionResponse_jobDefinitionArn,
    describeModelQualityJobDefinitionResponse_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_creationTime,
    describeModelQualityJobDefinitionResponse_modelQualityAppSpecification,
    describeModelQualityJobDefinitionResponse_modelQualityJobInput,
    describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig,
    describeModelQualityJobDefinitionResponse_jobResources,
    describeModelQualityJobDefinitionResponse_roleArn,

    -- ** DeleteStudioLifecycleConfig
    deleteStudioLifecycleConfig_studioLifecycleConfigName,

    -- ** DescribeModelExplainabilityJobDefinition
    describeModelExplainabilityJobDefinition_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_stoppingCondition,
    describeModelExplainabilityJobDefinitionResponse_networkConfig,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig,
    describeModelExplainabilityJobDefinitionResponse_httpStatus,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_creationTime,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig,
    describeModelExplainabilityJobDefinitionResponse_jobResources,
    describeModelExplainabilityJobDefinitionResponse_roleArn,

    -- ** StopNotebookInstance
    stopNotebookInstance_notebookInstanceName,

    -- ** UpdateEndpointWeightsAndCapacities
    updateEndpointWeightsAndCapacities_endpointName,
    updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities,
    updateEndpointWeightsAndCapacitiesResponse_httpStatus,
    updateEndpointWeightsAndCapacitiesResponse_endpointArn,

    -- ** CreateAppImageConfig
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_tags,
    createAppImageConfig_appImageConfigName,
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_resourceArn,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** ListExperiments
    listExperiments_createdAfter,
    listExperiments_nextToken,
    listExperiments_sortOrder,
    listExperiments_maxResults,
    listExperiments_createdBefore,
    listExperiments_sortBy,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_serviceCatalogProvisionedProductDetails,
    describeProjectResponse_createdBy,
    describeProjectResponse_projectDescription,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_projectId,
    describeProjectResponse_serviceCatalogProvisioningDetails,
    describeProjectResponse_projectStatus,
    describeProjectResponse_creationTime,

    -- ** ListAutoMLJobs
    listAutoMLJobs_nameContains,
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_creationTimeAfter,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_statusEquals,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_sortBy,
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,

    -- ** ListApps
    listApps_domainIdEquals,
    listApps_nextToken,
    listApps_sortOrder,
    listApps_userProfileNameEquals,
    listApps_maxResults,
    listApps_sortBy,
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,

    -- ** RetryPipelineExecution
    retryPipelineExecution_pipelineExecutionArn,
    retryPipelineExecution_clientRequestToken,
    retryPipelineExecutionResponse_pipelineExecutionArn,
    retryPipelineExecutionResponse_httpStatus,

    -- ** CreateProcessingJob
    createProcessingJob_environment,
    createProcessingJob_stoppingCondition,
    createProcessingJob_experimentConfig,
    createProcessingJob_processingInputs,
    createProcessingJob_networkConfig,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_tags,
    createProcessingJob_processingJobName,
    createProcessingJob_processingResources,
    createProcessingJob_appSpecification,
    createProcessingJob_roleArn,
    createProcessingJobResponse_httpStatus,
    createProcessingJobResponse_processingJobArn,

    -- ** DeleteMonitoringSchedule
    deleteMonitoringSchedule_monitoringScheduleName,

    -- ** DescribeModelPackage
    describeModelPackage_modelPackageName,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_validationSpecification,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_httpStatus,
    describeModelPackageResponse_modelPackageName,
    describeModelPackageResponse_modelPackageArn,
    describeModelPackageResponse_creationTime,
    describeModelPackageResponse_modelPackageStatus,
    describeModelPackageResponse_modelPackageStatusDetails,

    -- ** DeleteEndpointConfig
    deleteEndpointConfig_endpointConfigName,

    -- ** UpdateMonitoringSchedule
    updateMonitoringSchedule_monitoringScheduleName,
    updateMonitoringSchedule_monitoringScheduleConfig,
    updateMonitoringScheduleResponse_httpStatus,
    updateMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** AddAssociation
    addAssociation_associationType,
    addAssociation_sourceArn,
    addAssociation_destinationArn,
    addAssociationResponse_sourceArn,
    addAssociationResponse_destinationArn,
    addAssociationResponse_httpStatus,

    -- ** StartPipelineExecution
    startPipelineExecution_pipelineParameters,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineName,
    startPipelineExecution_clientRequestToken,
    startPipelineExecutionResponse_pipelineExecutionArn,
    startPipelineExecutionResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_domainId,
    deleteApp_userProfileName,
    deleteApp_appType,
    deleteApp_appName,

    -- ** CreateAlgorithm
    createAlgorithm_validationSpecification,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_algorithmDescription,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_tags,
    createAlgorithm_algorithmName,
    createAlgorithm_trainingSpecification,
    createAlgorithmResponse_httpStatus,
    createAlgorithmResponse_algorithmArn,

    -- ** ListPipelineExecutionSteps
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_pipelineDisplayName,
    updatePipeline_pipelineDescription,
    updatePipeline_pipelineDefinition,
    updatePipeline_roleArn,
    updatePipeline_pipelineName,
    updatePipelineResponse_pipelineArn,
    updatePipelineResponse_httpStatus,

    -- ** StopTransformJob
    stopTransformJob_transformJobName,

    -- ** DeletePipeline
    deletePipeline_pipelineName,
    deletePipeline_clientRequestToken,
    deletePipelineResponse_pipelineArn,
    deletePipelineResponse_httpStatus,

    -- ** DescribeAction
    describeAction_actionName,
    describeActionResponse_creationTime,
    describeActionResponse_metadataProperties,
    describeActionResponse_status,
    describeActionResponse_createdBy,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_actionName,
    describeActionResponse_source,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_actionArn,
    describeActionResponse_description,
    describeActionResponse_actionType,
    describeActionResponse_properties,
    describeActionResponse_httpStatus,

    -- ** CreateModel
    createModel_primaryContainer,
    createModel_enableNetworkIsolation,
    createModel_containers,
    createModel_vpcConfig,
    createModel_inferenceExecutionConfig,
    createModel_tags,
    createModel_modelName,
    createModel_executionRoleArn,
    createModelResponse_httpStatus,
    createModelResponse_modelArn,

    -- ** ListUserProfiles
    listUserProfiles_domainIdEquals,
    listUserProfiles_userProfileNameContains,
    listUserProfiles_nextToken,
    listUserProfiles_sortOrder,
    listUserProfiles_maxResults,
    listUserProfiles_sortBy,
    listUserProfilesResponse_userProfiles,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_httpStatus,

    -- ** CreateDataQualityJobDefinition
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_tags,
    createDataQualityJobDefinition_jobDefinitionName,
    createDataQualityJobDefinition_dataQualityAppSpecification,
    createDataQualityJobDefinition_dataQualityJobInput,
    createDataQualityJobDefinition_dataQualityJobOutputConfig,
    createDataQualityJobDefinition_jobResources,
    createDataQualityJobDefinition_roleArn,
    createDataQualityJobDefinitionResponse_httpStatus,
    createDataQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** DeleteModelPackageGroup
    deleteModelPackageGroup_modelPackageGroupName,

    -- ** DescribeArtifact
    describeArtifact_artifactArn,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_source,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_properties,
    describeArtifactResponse_httpStatus,

    -- ** StopEdgePackagingJob
    stopEdgePackagingJob_edgePackagingJobName,

    -- ** CreateCodeRepository
    createCodeRepository_tags,
    createCodeRepository_codeRepositoryName,
    createCodeRepository_gitConfig,
    createCodeRepositoryResponse_httpStatus,
    createCodeRepositoryResponse_codeRepositoryArn,

    -- ** CreateHyperParameterTuningJob
    createHyperParameterTuningJob_trainingJobDefinition,
    createHyperParameterTuningJob_warmStartConfig,
    createHyperParameterTuningJob_tags,
    createHyperParameterTuningJob_trainingJobDefinitions,
    createHyperParameterTuningJob_hyperParameterTuningJobName,
    createHyperParameterTuningJob_hyperParameterTuningJobConfig,
    createHyperParameterTuningJobResponse_httpStatus,
    createHyperParameterTuningJobResponse_hyperParameterTuningJobArn,

    -- ** DeleteTrial
    deleteTrial_trialName,
    deleteTrialResponse_trialArn,
    deleteTrialResponse_httpStatus,

    -- ** UpdateTrial
    updateTrial_displayName,
    updateTrial_trialName,
    updateTrialResponse_trialArn,
    updateTrialResponse_httpStatus,

    -- ** DescribeDeviceFleet
    describeDeviceFleet_deviceFleetName,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,

    -- ** ListCodeRepositories
    listCodeRepositories_nameContains,
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_creationTimeAfter,
    listCodeRepositories_nextToken,
    listCodeRepositories_sortOrder,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_maxResults,
    listCodeRepositories_sortBy,
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,

    -- ** DescribeCompilationJob
    describeCompilationJob_compilationJobName,
    describeCompilationJobResponse_modelDigests,
    describeCompilationJobResponse_compilationStartTime,
    describeCompilationJobResponse_inferenceImage,
    describeCompilationJobResponse_vpcConfig,
    describeCompilationJobResponse_compilationEndTime,
    describeCompilationJobResponse_httpStatus,
    describeCompilationJobResponse_compilationJobName,
    describeCompilationJobResponse_compilationJobArn,
    describeCompilationJobResponse_compilationJobStatus,
    describeCompilationJobResponse_stoppingCondition,
    describeCompilationJobResponse_creationTime,
    describeCompilationJobResponse_lastModifiedTime,
    describeCompilationJobResponse_failureReason,
    describeCompilationJobResponse_modelArtifacts,
    describeCompilationJobResponse_roleArn,
    describeCompilationJobResponse_inputConfig,
    describeCompilationJobResponse_outputConfig,

    -- ** ListPipelines
    listPipelines_createdAfter,
    listPipelines_pipelineNamePrefix,
    listPipelines_nextToken,
    listPipelines_sortOrder,
    listPipelines_maxResults,
    listPipelines_createdBefore,
    listPipelines_sortBy,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_httpStatus,

    -- ** ListHyperParameterTuningJobs
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_creationTimeAfter,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,

    -- ** ListAlgorithms
    listAlgorithms_nameContains,
    listAlgorithms_creationTimeAfter,
    listAlgorithms_nextToken,
    listAlgorithms_sortOrder,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_maxResults,
    listAlgorithms_sortBy,
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,

    -- ** CreateModelPackageGroup
    createModelPackageGroup_modelPackageGroupDescription,
    createModelPackageGroup_tags,
    createModelPackageGroup_modelPackageGroupName,
    createModelPackageGroupResponse_httpStatus,
    createModelPackageGroupResponse_modelPackageGroupArn,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    getSagemakerServicecatalogPortfolioStatusResponse_status,
    getSagemakerServicecatalogPortfolioStatusResponse_httpStatus,

    -- ** DescribeFeatureGroup
    describeFeatureGroup_nextToken,
    describeFeatureGroup_featureGroupName,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_onlineStoreConfig,
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_roleArn,
    describeFeatureGroupResponse_httpStatus,
    describeFeatureGroupResponse_featureGroupArn,
    describeFeatureGroupResponse_featureGroupName,
    describeFeatureGroupResponse_recordIdentifierFeatureName,
    describeFeatureGroupResponse_eventTimeFeatureName,
    describeFeatureGroupResponse_featureDefinitions,
    describeFeatureGroupResponse_creationTime,
    describeFeatureGroupResponse_nextToken,

    -- ** RenderUiTemplate
    renderUiTemplate_uiTemplate,
    renderUiTemplate_humanTaskUiArn,
    renderUiTemplate_task,
    renderUiTemplate_roleArn,
    renderUiTemplateResponse_httpStatus,
    renderUiTemplateResponse_renderedContent,
    renderUiTemplateResponse_errors,

    -- ** DeleteFlowDefinition
    deleteFlowDefinition_flowDefinitionName,
    deleteFlowDefinitionResponse_httpStatus,

    -- ** SendPipelineExecutionStepFailure
    sendPipelineExecutionStepFailure_failureReason,
    sendPipelineExecutionStepFailure_clientRequestToken,
    sendPipelineExecutionStepFailure_callbackToken,
    sendPipelineExecutionStepFailureResponse_pipelineExecutionArn,
    sendPipelineExecutionStepFailureResponse_httpStatus,

    -- ** CreateTrial
    createTrial_metadataProperties,
    createTrial_displayName,
    createTrial_tags,
    createTrial_trialName,
    createTrial_experimentName,
    createTrialResponse_trialArn,
    createTrialResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** ListDataQualityJobDefinitions
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_creationTimeAfter,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_sortBy,
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModels
    listModels_nameContains,
    listModels_creationTimeAfter,
    listModels_nextToken,
    listModels_sortOrder,
    listModels_creationTimeBefore,
    listModels_maxResults,
    listModels_sortBy,
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,

    -- ** DeleteAlgorithm
    deleteAlgorithm_algorithmName,

    -- ** AssociateTrialComponent
    associateTrialComponent_trialComponentName,
    associateTrialComponent_trialName,
    associateTrialComponentResponse_trialArn,
    associateTrialComponentResponse_trialComponentArn,
    associateTrialComponentResponse_httpStatus,

    -- ** UpdatePipelineExecution
    updatePipelineExecution_pipelineExecutionDisplayName,
    updatePipelineExecution_pipelineExecutionDescription,
    updatePipelineExecution_pipelineExecutionArn,
    updatePipelineExecutionResponse_pipelineExecutionArn,
    updatePipelineExecutionResponse_httpStatus,

    -- ** DescribeNotebookInstanceLifecycleConfig
    describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_creationTime,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** DescribeWorkforce
    describeWorkforce_workforceName,
    describeWorkforceResponse_httpStatus,
    describeWorkforceResponse_workforce,

    -- ** DeleteModelExplainabilityJobDefinition
    deleteModelExplainabilityJobDefinition_jobDefinitionName,

    -- ** CreateModelPackage
    createModelPackage_metadataProperties,
    createModelPackage_modelApprovalStatus,
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_modelPackageName,
    createModelPackage_clientToken,
    createModelPackage_modelMetrics,
    createModelPackage_modelPackageDescription,
    createModelPackage_validationSpecification,
    createModelPackage_inferenceSpecification,
    createModelPackage_certifyForMarketplace,
    createModelPackage_modelPackageGroupName,
    createModelPackage_tags,
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,

    -- ** DeleteModelQualityJobDefinition
    deleteModelQualityJobDefinition_jobDefinitionName,

    -- ** StopMonitoringSchedule
    stopMonitoringSchedule_monitoringScheduleName,

    -- ** ListModelExplainabilityJobDefinitions
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** DescribeAppImageConfig
    describeAppImageConfig_appImageConfigName,
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_httpStatus,

    -- ** ListNotebookInstances
    listNotebookInstances_nameContains,
    listNotebookInstances_defaultCodeRepositoryContains,
    listNotebookInstances_lastModifiedTimeBefore,
    listNotebookInstances_notebookInstanceLifecycleConfigNameContains,
    listNotebookInstances_creationTimeAfter,
    listNotebookInstances_additionalCodeRepositoryEquals,
    listNotebookInstances_nextToken,
    listNotebookInstances_sortOrder,
    listNotebookInstances_lastModifiedTimeAfter,
    listNotebookInstances_creationTimeBefore,
    listNotebookInstances_statusEquals,
    listNotebookInstances_maxResults,
    listNotebookInstances_sortBy,
    listNotebookInstancesResponse_notebookInstances,
    listNotebookInstancesResponse_nextToken,
    listNotebookInstancesResponse_httpStatus,

    -- ** DescribeStudioLifecycleConfig
    describeStudioLifecycleConfig_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_creationTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigContent,
    describeStudioLifecycleConfigResponse_lastModifiedTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_httpStatus,

    -- ** StopLabelingJob
    stopLabelingJob_labelingJobName,

    -- ** DeleteNotebookInstance
    deleteNotebookInstance_notebookInstanceName,

    -- ** UpdateNotebookInstance
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_roleArn,
    updateNotebookInstance_notebookInstanceName,
    updateNotebookInstanceResponse_httpStatus,

    -- ** ListModelPackages
    listModelPackages_nameContains,
    listModelPackages_modelApprovalStatus,
    listModelPackages_modelPackageType,
    listModelPackages_creationTimeAfter,
    listModelPackages_nextToken,
    listModelPackages_sortOrder,
    listModelPackages_modelPackageGroupName,
    listModelPackages_creationTimeBefore,
    listModelPackages_maxResults,
    listModelPackages_sortBy,
    listModelPackagesResponse_nextToken,
    listModelPackagesResponse_httpStatus,
    listModelPackagesResponse_modelPackageSummaryList,

    -- ** CreateModelQualityJobDefinition
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_tags,
    createModelQualityJobDefinition_jobDefinitionName,
    createModelQualityJobDefinition_modelQualityAppSpecification,
    createModelQualityJobDefinition_modelQualityJobInput,
    createModelQualityJobDefinition_modelQualityJobOutputConfig,
    createModelQualityJobDefinition_jobResources,
    createModelQualityJobDefinition_roleArn,
    createModelQualityJobDefinitionResponse_httpStatus,
    createModelQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** DeleteImageVersion
    deleteImageVersion_imageName,
    deleteImageVersion_version,
    deleteImageVersionResponse_httpStatus,

    -- ** DescribeExperiment
    describeExperiment_experimentName,
    describeExperimentResponse_creationTime,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_source,
    describeExperimentResponse_displayName,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_description,
    describeExperimentResponse_httpStatus,

    -- ** DeleteTrialComponent
    deleteTrialComponent_trialComponentName,
    deleteTrialComponentResponse_trialComponentArn,
    deleteTrialComponentResponse_httpStatus,

    -- ** UpdateTrialComponent
    updateTrialComponent_status,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_startTime,
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_endTime,
    updateTrialComponent_parameters,
    updateTrialComponent_displayName,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_trialComponentName,
    updateTrialComponentResponse_trialComponentArn,
    updateTrialComponentResponse_httpStatus,

    -- ** DescribeLabelingJob
    describeLabelingJob_labelingJobName,
    describeLabelingJobResponse_failureReason,
    describeLabelingJobResponse_labelingJobAlgorithmsConfig,
    describeLabelingJobResponse_labelCategoryConfigS3Uri,
    describeLabelingJobResponse_stoppingConditions,
    describeLabelingJobResponse_labelAttributeName,
    describeLabelingJobResponse_labelingJobOutput,
    describeLabelingJobResponse_tags,
    describeLabelingJobResponse_httpStatus,
    describeLabelingJobResponse_labelingJobStatus,
    describeLabelingJobResponse_labelCounters,
    describeLabelingJobResponse_creationTime,
    describeLabelingJobResponse_lastModifiedTime,
    describeLabelingJobResponse_jobReferenceCode,
    describeLabelingJobResponse_labelingJobName,
    describeLabelingJobResponse_labelingJobArn,
    describeLabelingJobResponse_inputConfig,
    describeLabelingJobResponse_outputConfig,
    describeLabelingJobResponse_roleArn,
    describeLabelingJobResponse_humanTaskConfig,

    -- ** CreateDomain
    createDomain_homeEfsFileSystemKmsKeyId,
    createDomain_kmsKeyId,
    createDomain_appNetworkAccessType,
    createDomain_tags,
    createDomain_domainName,
    createDomain_authMode,
    createDomain_defaultUserSettings,
    createDomain_subnetIds,
    createDomain_vpcId,
    createDomainResponse_domainArn,
    createDomainResponse_url,
    createDomainResponse_httpStatus,

    -- ** ListDeviceFleets
    listDeviceFleets_nameContains,
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_creationTimeAfter,
    listDeviceFleets_nextToken,
    listDeviceFleets_sortOrder,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_maxResults,
    listDeviceFleets_sortBy,
    listDeviceFleetsResponse_nextToken,
    listDeviceFleetsResponse_httpStatus,
    listDeviceFleetsResponse_deviceFleetSummaries,

    -- ** DescribeUserProfile
    describeUserProfile_domainId,
    describeUserProfile_userProfileName,
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_status,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_httpStatus,

    -- ** ListMonitoringExecutions
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutions_statusEquals,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_monitoringJobDefinitionName,
    listMonitoringExecutionsResponse_nextToken,
    listMonitoringExecutionsResponse_httpStatus,
    listMonitoringExecutionsResponse_monitoringExecutionSummaries,

    -- ** DeleteHumanTaskUi
    deleteHumanTaskUi_humanTaskUiName,
    deleteHumanTaskUiResponse_httpStatus,

    -- ** StopTrainingJob
    stopTrainingJob_trainingJobName,

    -- ** CreateFeatureGroup
    createFeatureGroup_offlineStoreConfig,
    createFeatureGroup_onlineStoreConfig,
    createFeatureGroup_description,
    createFeatureGroup_tags,
    createFeatureGroup_roleArn,
    createFeatureGroup_featureGroupName,
    createFeatureGroup_recordIdentifierFeatureName,
    createFeatureGroup_eventTimeFeatureName,
    createFeatureGroup_featureDefinitions,
    createFeatureGroupResponse_httpStatus,
    createFeatureGroupResponse_featureGroupArn,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmName,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_algorithmDescription,
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,

    -- ** UpdateDevices
    updateDevices_deviceFleetName,
    updateDevices_devices,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_primaryContainer,
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_containers,
    describeModelResponse_vpcConfig,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,

    -- ** ListTransformJobs
    listTransformJobs_nameContains,
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_creationTimeAfter,
    listTransformJobs_nextToken,
    listTransformJobs_sortOrder,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_statusEquals,
    listTransformJobs_maxResults,
    listTransformJobs_sortBy,
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,

    -- ** DeleteFeatureGroup
    deleteFeatureGroup_featureGroupName,

    -- ** ListEdgePackagingJobs
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_creationTimeAfter,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_statusEquals,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,

    -- ** DescribeHyperParameterTuningJob
    describeHyperParameterTuningJob_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_failureReason,
    describeHyperParameterTuningJobResponse_trainingJobDefinition,
    describeHyperParameterTuningJobResponse_lastModifiedTime,
    describeHyperParameterTuningJobResponse_bestTrainingJob,
    describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime,
    describeHyperParameterTuningJobResponse_overallBestTrainingJob,
    describeHyperParameterTuningJobResponse_warmStartConfig,
    describeHyperParameterTuningJobResponse_trainingJobDefinitions,
    describeHyperParameterTuningJobResponse_httpStatus,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus,
    describeHyperParameterTuningJobResponse_creationTime,
    describeHyperParameterTuningJobResponse_trainingJobStatusCounters,
    describeHyperParameterTuningJobResponse_objectiveStatusCounters,

    -- ** ListEndpoints
    listEndpoints_nameContains,
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_creationTimeAfter,
    listEndpoints_nextToken,
    listEndpoints_sortOrder,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_creationTimeBefore,
    listEndpoints_statusEquals,
    listEndpoints_maxResults,
    listEndpoints_sortBy,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
    listEndpointsResponse_endpoints,

    -- ** DescribeFlowDefinition
    describeFlowDefinition_flowDefinitionName,
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_humanLoopActivationConfig,
    describeFlowDefinitionResponse_httpStatus,
    describeFlowDefinitionResponse_flowDefinitionArn,
    describeFlowDefinitionResponse_flowDefinitionName,
    describeFlowDefinitionResponse_flowDefinitionStatus,
    describeFlowDefinitionResponse_creationTime,
    describeFlowDefinitionResponse_humanLoopConfig,
    describeFlowDefinitionResponse_outputConfig,
    describeFlowDefinitionResponse_roleArn,

    -- ** CreateDeviceFleet
    createDeviceFleet_enableIotRoleAlias,
    createDeviceFleet_description,
    createDeviceFleet_tags,
    createDeviceFleet_roleArn,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- ** CreatePresignedNotebookInstanceUrl
    createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds,
    createPresignedNotebookInstanceUrl_notebookInstanceName,
    createPresignedNotebookInstanceUrlResponse_authorizedUrl,
    createPresignedNotebookInstanceUrlResponse_httpStatus,

    -- ** ListTrainingJobsForHyperParameterTuningJob
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
    listTrainingJobsForHyperParameterTuningJob_statusEquals,
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName,
    listTrainingJobsForHyperParameterTuningJobResponse_nextToken,
    listTrainingJobsForHyperParameterTuningJobResponse_httpStatus,
    listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries,

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_creationTime,
    describeDomainResponse_status,
    describeDomainResponse_failureReason,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_subnetIds,
    describeDomainResponse_domainArn,
    describeDomainResponse_vpcId,
    describeDomainResponse_url,
    describeDomainResponse_authMode,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_domainName,
    describeDomainResponse_domainId,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_httpStatus,

    -- ** DeleteModelBiasJobDefinition
    deleteModelBiasJobDefinition_jobDefinitionName,

    -- ** UpdateWorkteam
    updateWorkteam_notificationConfiguration,
    updateWorkteam_memberDefinitions,
    updateWorkteam_description,
    updateWorkteam_workteamName,
    updateWorkteamResponse_httpStatus,
    updateWorkteamResponse_workteam,

    -- ** DeleteWorkteam
    deleteWorkteam_workteamName,
    deleteWorkteamResponse_httpStatus,
    deleteWorkteamResponse_success,

    -- ** ListWorkteams
    listWorkteams_nameContains,
    listWorkteams_nextToken,
    listWorkteams_sortOrder,
    listWorkteams_maxResults,
    listWorkteams_sortBy,
    listWorkteamsResponse_nextToken,
    listWorkteamsResponse_httpStatus,
    listWorkteamsResponse_workteams,

    -- ** DescribeDevice
    describeDevice_nextToken,
    describeDevice_deviceName,
    describeDevice_deviceFleetName,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_nextToken,
    describeDeviceResponse_models,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_description,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,

    -- ** CreateAutoMLJob
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_problemType,
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_modelDeployConfig,
    createAutoMLJob_tags,
    createAutoMLJob_autoMLJobName,
    createAutoMLJob_inputDataConfig,
    createAutoMLJob_outputDataConfig,
    createAutoMLJob_roleArn,
    createAutoMLJobResponse_httpStatus,
    createAutoMLJobResponse_autoMLJobArn,

    -- ** CreateApp
    createApp_resourceSpec,
    createApp_tags,
    createApp_domainId,
    createApp_userProfileName,
    createApp_appType,
    createApp_appName,
    createAppResponse_appArn,
    createAppResponse_httpStatus,

    -- ** CreateExperiment
    createExperiment_displayName,
    createExperiment_description,
    createExperiment_tags,
    createExperiment_experimentName,
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,

    -- ** ListNotebookInstanceLifecycleConfigs
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_sortBy,
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,

    -- ** ListWorkforces
    listWorkforces_nameContains,
    listWorkforces_nextToken,
    listWorkforces_sortOrder,
    listWorkforces_maxResults,
    listWorkforces_sortBy,
    listWorkforcesResponse_nextToken,
    listWorkforcesResponse_httpStatus,
    listWorkforcesResponse_workforces,

    -- ** DescribeSubscribedWorkteam
    describeSubscribedWorkteam_workteamArn,
    describeSubscribedWorkteamResponse_httpStatus,
    describeSubscribedWorkteamResponse_subscribedWorkteam,

    -- ** ListStudioLifecycleConfigs
    listStudioLifecycleConfigs_nameContains,
    listStudioLifecycleConfigs_creationTimeAfter,
    listStudioLifecycleConfigs_modifiedTimeAfter,
    listStudioLifecycleConfigs_nextToken,
    listStudioLifecycleConfigs_sortOrder,
    listStudioLifecycleConfigs_creationTimeBefore,
    listStudioLifecycleConfigs_modifiedTimeBefore,
    listStudioLifecycleConfigs_appTypeEquals,
    listStudioLifecycleConfigs_maxResults,
    listStudioLifecycleConfigs_sortBy,
    listStudioLifecycleConfigsResponse_nextToken,
    listStudioLifecycleConfigsResponse_studioLifecycleConfigs,
    listStudioLifecycleConfigsResponse_httpStatus,

    -- ** ListModelBiasJobDefinitions
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_creationTimeAfter,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_sortBy,
    listModelBiasJobDefinitionsResponse_nextToken,
    listModelBiasJobDefinitionsResponse_httpStatus,
    listModelBiasJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** CreateStudioLifecycleConfig
    createStudioLifecycleConfig_tags,
    createStudioLifecycleConfig_studioLifecycleConfigName,
    createStudioLifecycleConfig_studioLifecycleConfigContent,
    createStudioLifecycleConfig_studioLifecycleConfigAppType,
    createStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    createStudioLifecycleConfigResponse_httpStatus,

    -- ** DisableSagemakerServicecatalogPortfolio
    disableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** CreateWorkteam
    createWorkteam_notificationConfiguration,
    createWorkteam_workforceName,
    createWorkteam_tags,
    createWorkteam_workteamName,
    createWorkteam_memberDefinitions,
    createWorkteam_description,
    createWorkteamResponse_workteamArn,
    createWorkteamResponse_httpStatus,

    -- ** CreateNotebookInstanceLifecycleConfig
    createNotebookInstanceLifecycleConfig_onCreate,
    createNotebookInstanceLifecycleConfig_onStart,
    createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    createNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** ListMonitoringSchedules
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,

    -- ** ListLabelingJobs
    listLabelingJobs_nameContains,
    listLabelingJobs_lastModifiedTimeBefore,
    listLabelingJobs_creationTimeAfter,
    listLabelingJobs_nextToken,
    listLabelingJobs_sortOrder,
    listLabelingJobs_lastModifiedTimeAfter,
    listLabelingJobs_creationTimeBefore,
    listLabelingJobs_statusEquals,
    listLabelingJobs_maxResults,
    listLabelingJobs_sortBy,
    listLabelingJobsResponse_labelingJobSummaryList,
    listLabelingJobsResponse_nextToken,
    listLabelingJobsResponse_httpStatus,

    -- ** StartNotebookInstance
    startNotebookInstance_notebookInstanceName,

    -- ** UpdateExperiment
    updateExperiment_displayName,
    updateExperiment_description,
    updateExperiment_experimentName,
    updateExperimentResponse_experimentArn,
    updateExperimentResponse_httpStatus,

    -- ** DeleteExperiment
    deleteExperiment_experimentName,
    deleteExperimentResponse_experimentArn,
    deleteExperimentResponse_httpStatus,

    -- ** StopPipelineExecution
    stopPipelineExecution_pipelineExecutionArn,
    stopPipelineExecution_clientRequestToken,
    stopPipelineExecutionResponse_pipelineExecutionArn,
    stopPipelineExecutionResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArn,
    addTags_tags,
    addTagsResponse_tags,
    addTagsResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_createdAfter,
    listAssociations_sourceType,
    listAssociations_sourceArn,
    listAssociations_associationType,
    listAssociations_destinationArn,
    listAssociations_nextToken,
    listAssociations_destinationType,
    listAssociations_sortOrder,
    listAssociations_maxResults,
    listAssociations_createdBefore,
    listAssociations_sortBy,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_httpStatus,

    -- ** CreateWorkforce
    createWorkforce_sourceIpConfig,
    createWorkforce_cognitoConfig,
    createWorkforce_oidcConfig,
    createWorkforce_tags,
    createWorkforce_workforceName,
    createWorkforceResponse_httpStatus,
    createWorkforceResponse_workforceArn,

    -- ** DescribeTrialComponent
    describeTrialComponent_trialComponentName,
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_httpStatus,

    -- ** DescribeImageVersion
    describeImageVersion_version,
    describeImageVersion_imageName,
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_version,
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_httpStatus,

    -- ** CreateModelBiasJobDefinition
    createModelBiasJobDefinition_modelBiasBaselineConfig,
    createModelBiasJobDefinition_stoppingCondition,
    createModelBiasJobDefinition_networkConfig,
    createModelBiasJobDefinition_tags,
    createModelBiasJobDefinition_jobDefinitionName,
    createModelBiasJobDefinition_modelBiasAppSpecification,
    createModelBiasJobDefinition_modelBiasJobInput,
    createModelBiasJobDefinition_modelBiasJobOutputConfig,
    createModelBiasJobDefinition_jobResources,
    createModelBiasJobDefinition_roleArn,
    createModelBiasJobDefinitionResponse_httpStatus,
    createModelBiasJobDefinitionResponse_jobDefinitionArn,

    -- ** ListEndpointConfigs
    listEndpointConfigs_nameContains,
    listEndpointConfigs_creationTimeAfter,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_sortOrder,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_sortBy,
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,

    -- ** DeleteAssociation
    deleteAssociation_sourceArn,
    deleteAssociation_destinationArn,
    deleteAssociationResponse_sourceArn,
    deleteAssociationResponse_destinationArn,
    deleteAssociationResponse_httpStatus,

    -- ** CreateFlowDefinition
    createFlowDefinition_humanLoopRequestSource,
    createFlowDefinition_humanLoopActivationConfig,
    createFlowDefinition_tags,
    createFlowDefinition_flowDefinitionName,
    createFlowDefinition_humanLoopConfig,
    createFlowDefinition_outputConfig,
    createFlowDefinition_roleArn,
    createFlowDefinitionResponse_httpStatus,
    createFlowDefinitionResponse_flowDefinitionArn,

    -- ** ListModelPackageGroups
    listModelPackageGroups_nameContains,
    listModelPackageGroups_creationTimeAfter,
    listModelPackageGroups_nextToken,
    listModelPackageGroups_sortOrder,
    listModelPackageGroups_creationTimeBefore,
    listModelPackageGroups_maxResults,
    listModelPackageGroups_sortBy,
    listModelPackageGroupsResponse_nextToken,
    listModelPackageGroupsResponse_httpStatus,
    listModelPackageGroupsResponse_modelPackageGroupSummaryList,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeregisterDevices
    deregisterDevices_deviceFleetName,
    deregisterDevices_deviceNames,

    -- ** DescribeHumanTaskUi
    describeHumanTaskUi_humanTaskUiName,
    describeHumanTaskUiResponse_humanTaskUiStatus,
    describeHumanTaskUiResponse_httpStatus,
    describeHumanTaskUiResponse_humanTaskUiArn,
    describeHumanTaskUiResponse_humanTaskUiName,
    describeHumanTaskUiResponse_creationTime,
    describeHumanTaskUiResponse_uiTemplate,

    -- ** CreateTrainingJob
    createTrainingJob_environment,
    createTrainingJob_debugHookConfig,
    createTrainingJob_checkpointConfig,
    createTrainingJob_retryStrategy,
    createTrainingJob_profilerConfig,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_experimentConfig,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_hyperParameters,
    createTrainingJob_inputDataConfig,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_vpcConfig,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_tags,
    createTrainingJob_trainingJobName,
    createTrainingJob_algorithmSpecification,
    createTrainingJob_roleArn,
    createTrainingJob_outputDataConfig,
    createTrainingJob_resourceConfig,
    createTrainingJob_stoppingCondition,
    createTrainingJobResponse_httpStatus,
    createTrainingJobResponse_trainingJobArn,

    -- ** DeleteModelPackageGroupPolicy
    deleteModelPackageGroupPolicy_modelPackageGroupName,

    -- ** DeleteUserProfile
    deleteUserProfile_domainId,
    deleteUserProfile_userProfileName,

    -- ** UpdateUserProfile
    updateUserProfile_userSettings,
    updateUserProfile_domainId,
    updateUserProfile_userProfileName,
    updateUserProfileResponse_userProfileArn,
    updateUserProfileResponse_httpStatus,

    -- ** CreateImage
    createImage_displayName,
    createImage_description,
    createImage_tags,
    createImage_imageName,
    createImage_roleArn,
    createImageResponse_imageArn,
    createImageResponse_httpStatus,

    -- ** PutModelPackageGroupPolicy
    putModelPackageGroupPolicy_modelPackageGroupName,
    putModelPackageGroupPolicy_resourcePolicy,
    putModelPackageGroupPolicyResponse_httpStatus,
    putModelPackageGroupPolicyResponse_modelPackageGroupArn,

    -- ** ListPipelineParametersForExecution
    listPipelineParametersForExecution_nextToken,
    listPipelineParametersForExecution_maxResults,
    listPipelineParametersForExecution_pipelineExecutionArn,
    listPipelineParametersForExecutionResponse_pipelineParameters,
    listPipelineParametersForExecutionResponse_nextToken,
    listPipelineParametersForExecutionResponse_httpStatus,

    -- ** CreateContext
    createContext_description,
    createContext_tags,
    createContext_properties,
    createContext_contextName,
    createContext_source,
    createContext_contextType,
    createContextResponse_contextArn,
    createContextResponse_httpStatus,

    -- ** DescribePipelineDefinitionForExecution
    describePipelineDefinitionForExecution_pipelineExecutionArn,
    describePipelineDefinitionForExecutionResponse_creationTime,
    describePipelineDefinitionForExecutionResponse_pipelineDefinition,
    describePipelineDefinitionForExecutionResponse_httpStatus,

    -- ** ListTrials
    listTrials_createdAfter,
    listTrials_experimentName,
    listTrials_nextToken,
    listTrials_sortOrder,
    listTrials_trialComponentName,
    listTrials_maxResults,
    listTrials_createdBefore,
    listTrials_sortBy,
    listTrialsResponse_nextToken,
    listTrialsResponse_trialSummaries,
    listTrialsResponse_httpStatus,

    -- ** StopCompilationJob
    stopCompilationJob_compilationJobName,

    -- ** ListImages
    listImages_nameContains,
    listImages_lastModifiedTimeBefore,
    listImages_creationTimeAfter,
    listImages_nextToken,
    listImages_sortOrder,
    listImages_lastModifiedTimeAfter,
    listImages_creationTimeBefore,
    listImages_maxResults,
    listImages_sortBy,
    listImagesResponse_images,
    listImagesResponse_nextToken,
    listImagesResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_userSettings,
    createUserProfile_singleSignOnUserValue,
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_tags,
    createUserProfile_domainId,
    createUserProfile_userProfileName,
    createUserProfileResponse_userProfileArn,
    createUserProfileResponse_httpStatus,

    -- ** Search
    search_nextToken,
    search_searchExpression,
    search_sortOrder,
    search_maxResults,
    search_sortBy,
    search_resource,
    searchResponse_results,
    searchResponse_nextToken,
    searchResponse_httpStatus,

    -- ** UpdateCodeRepository
    updateCodeRepository_gitConfig,
    updateCodeRepository_codeRepositoryName,
    updateCodeRepositoryResponse_httpStatus,
    updateCodeRepositoryResponse_codeRepositoryArn,

    -- ** DeleteCodeRepository
    deleteCodeRepository_codeRepositoryName,

    -- ** ListContexts
    listContexts_createdAfter,
    listContexts_nextToken,
    listContexts_sortOrder,
    listContexts_contextType,
    listContexts_sourceUri,
    listContexts_maxResults,
    listContexts_createdBefore,
    listContexts_sortBy,
    listContextsResponse_contextSummaries,
    listContextsResponse_nextToken,
    listContextsResponse_httpStatus,

    -- ** DescribeTransformJob
    describeTransformJob_transformJobName,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_transformStartTime,
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_httpStatus,
    describeTransformJobResponse_transformJobName,
    describeTransformJobResponse_transformJobArn,
    describeTransformJobResponse_transformJobStatus,
    describeTransformJobResponse_modelName,
    describeTransformJobResponse_transformInput,
    describeTransformJobResponse_transformResources,
    describeTransformJobResponse_creationTime,

    -- ** DescribeEdgePackagingJob
    describeEdgePackagingJob_edgePackagingJobName,
    describeEdgePackagingJobResponse_creationTime,
    describeEdgePackagingJobResponse_resourceKey,
    describeEdgePackagingJobResponse_modelName,
    describeEdgePackagingJobResponse_modelSignature,
    describeEdgePackagingJobResponse_lastModifiedTime,
    describeEdgePackagingJobResponse_modelArtifact,
    describeEdgePackagingJobResponse_compilationJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatusMessage,
    describeEdgePackagingJobResponse_outputConfig,
    describeEdgePackagingJobResponse_modelVersion,
    describeEdgePackagingJobResponse_presetDeploymentOutput,
    describeEdgePackagingJobResponse_roleArn,
    describeEdgePackagingJobResponse_httpStatus,
    describeEdgePackagingJobResponse_edgePackagingJobArn,
    describeEdgePackagingJobResponse_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatus,

    -- ** CreatePipeline
    createPipeline_pipelineDisplayName,
    createPipeline_pipelineDescription,
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_pipelineDefinition,
    createPipeline_clientRequestToken,
    createPipeline_roleArn,
    createPipelineResponse_pipelineArn,
    createPipelineResponse_httpStatus,

    -- ** ListCandidatesForAutoMLJob
    listCandidatesForAutoMLJob_candidateNameEquals,
    listCandidatesForAutoMLJob_nextToken,
    listCandidatesForAutoMLJob_sortOrder,
    listCandidatesForAutoMLJob_statusEquals,
    listCandidatesForAutoMLJob_maxResults,
    listCandidatesForAutoMLJob_sortBy,
    listCandidatesForAutoMLJob_autoMLJobName,
    listCandidatesForAutoMLJobResponse_nextToken,
    listCandidatesForAutoMLJobResponse_httpStatus,
    listCandidatesForAutoMLJobResponse_candidates,

    -- ** DeleteImage
    deleteImage_imageName,
    deleteImageResponse_httpStatus,

    -- ** UpdateImage
    updateImage_deleteProperties,
    updateImage_displayName,
    updateImage_description,
    updateImage_roleArn,
    updateImage_imageName,
    updateImageResponse_imageArn,
    updateImageResponse_httpStatus,

    -- ** ListFlowDefinitions
    listFlowDefinitions_creationTimeAfter,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_sortOrder,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_maxResults,
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,

    -- ** DeleteContext
    deleteContext_contextName,
    deleteContextResponse_contextArn,
    deleteContextResponse_httpStatus,

    -- ** UpdateContext
    updateContext_propertiesToRemove,
    updateContext_description,
    updateContext_properties,
    updateContext_contextName,
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointName,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_asyncInferenceConfig,
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_httpStatus,
    describeEndpointResponse_endpointName,
    describeEndpointResponse_endpointArn,
    describeEndpointResponse_endpointConfigName,
    describeEndpointResponse_endpointStatus,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_lastModifiedTime,

    -- ** UpdateTrainingJob
    updateTrainingJob_profilerConfig,
    updateTrainingJob_profilerRuleConfigurations,
    updateTrainingJob_trainingJobName,
    updateTrainingJobResponse_httpStatus,
    updateTrainingJobResponse_trainingJobArn,

    -- ** ListTrainingJobs
    listTrainingJobs_nameContains,
    listTrainingJobs_lastModifiedTimeBefore,
    listTrainingJobs_creationTimeAfter,
    listTrainingJobs_nextToken,
    listTrainingJobs_sortOrder,
    listTrainingJobs_lastModifiedTimeAfter,
    listTrainingJobs_creationTimeBefore,
    listTrainingJobs_statusEquals,
    listTrainingJobs_maxResults,
    listTrainingJobs_sortBy,
    listTrainingJobsResponse_nextToken,
    listTrainingJobsResponse_httpStatus,
    listTrainingJobsResponse_trainingJobSummaries,

    -- ** GetDeviceFleetReport
    getDeviceFleetReport_deviceFleetName,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_modelStats,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,

    -- ** DeleteDataQualityJobDefinition
    deleteDataQualityJobDefinition_jobDefinitionName,

    -- ** DescribeWorkteam
    describeWorkteam_workteamName,
    describeWorkteamResponse_httpStatus,
    describeWorkteamResponse_workteam,

    -- ** ListSubscribedWorkteams
    listSubscribedWorkteams_nameContains,
    listSubscribedWorkteams_nextToken,
    listSubscribedWorkteams_maxResults,
    listSubscribedWorkteamsResponse_nextToken,
    listSubscribedWorkteamsResponse_httpStatus,
    listSubscribedWorkteamsResponse_subscribedWorkteams,

    -- ** DeleteDomain
    deleteDomain_retentionPolicy,
    deleteDomain_domainId,

    -- ** UpdateDomain
    updateDomain_defaultUserSettings,
    updateDomain_domainId,
    updateDomainResponse_domainArn,
    updateDomainResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_httpStatus,

    -- ** ListModelQualityJobDefinitions
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_creationTimeAfter,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_sortOrder,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** CreateImageVersion
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,

    -- ** ListDevices
    listDevices_latestHeartbeatAfter,
    listDevices_modelName,
    listDevices_nextToken,
    listDevices_maxResults,
    listDevices_deviceFleetName,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,

    -- ** ListPipelineExecutions
    listPipelineExecutions_createdAfter,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_sortOrder,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_createdBefore,
    listPipelineExecutions_sortBy,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** CreateProject
    createProject_projectDescription,
    createProject_tags,
    createProject_projectName,
    createProject_serviceCatalogProvisioningDetails,
    createProjectResponse_httpStatus,
    createProjectResponse_projectArn,
    createProjectResponse_projectId,

    -- ** DescribeModelBiasJobDefinition
    describeModelBiasJobDefinition_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig,
    describeModelBiasJobDefinitionResponse_stoppingCondition,
    describeModelBiasJobDefinitionResponse_networkConfig,
    describeModelBiasJobDefinitionResponse_httpStatus,
    describeModelBiasJobDefinitionResponse_jobDefinitionArn,
    describeModelBiasJobDefinitionResponse_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_creationTime,
    describeModelBiasJobDefinitionResponse_modelBiasAppSpecification,
    describeModelBiasJobDefinitionResponse_modelBiasJobInput,
    describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig,
    describeModelBiasJobDefinitionResponse_jobResources,
    describeModelBiasJobDefinitionResponse_roleArn,

    -- ** StartMonitoringSchedule
    startMonitoringSchedule_monitoringScheduleName,

    -- ** StopAutoMLJob
    stopAutoMLJob_autoMLJobName,

    -- ** CreateTrialComponent
    createTrialComponent_metadataProperties,
    createTrialComponent_status,
    createTrialComponent_outputArtifacts,
    createTrialComponent_startTime,
    createTrialComponent_endTime,
    createTrialComponent_parameters,
    createTrialComponent_displayName,
    createTrialComponent_inputArtifacts,
    createTrialComponent_tags,
    createTrialComponent_trialComponentName,
    createTrialComponentResponse_trialComponentArn,
    createTrialComponentResponse_httpStatus,

    -- ** DescribeProcessingJob
    describeProcessingJob_processingJobName,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,

    -- * Types

    -- ** ActionSource
    actionSource_sourceType,
    actionSource_sourceId,
    actionSource_sourceUri,

    -- ** ActionSummary
    actionSummary_creationTime,
    actionSummary_status,
    actionSummary_lastModifiedTime,
    actionSummary_actionName,
    actionSummary_source,
    actionSummary_actionArn,
    actionSummary_actionType,

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_agentCount,

    -- ** Alarm
    alarm_alarmName,

    -- ** AlgorithmSpecification
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_algorithmName,
    algorithmSpecification_trainingImage,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_trainingInputMode,

    -- ** AlgorithmStatusDetails
    algorithmStatusDetails_imageScanStatuses,
    algorithmStatusDetails_validationStatuses,

    -- ** AlgorithmStatusItem
    algorithmStatusItem_failureReason,
    algorithmStatusItem_name,
    algorithmStatusItem_status,

    -- ** AlgorithmSummary
    algorithmSummary_algorithmDescription,
    algorithmSummary_algorithmName,
    algorithmSummary_algorithmArn,
    algorithmSummary_creationTime,
    algorithmSummary_algorithmStatus,

    -- ** AlgorithmValidationProfile
    algorithmValidationProfile_transformJobDefinition,
    algorithmValidationProfile_profileName,
    algorithmValidationProfile_trainingJobDefinition,

    -- ** AlgorithmValidationSpecification
    algorithmValidationSpecification_validationRole,
    algorithmValidationSpecification_validationProfiles,

    -- ** AnnotationConsolidationConfig
    annotationConsolidationConfig_annotationConsolidationLambdaArn,

    -- ** AppDetails
    appDetails_creationTime,
    appDetails_status,
    appDetails_userProfileName,
    appDetails_appName,
    appDetails_domainId,
    appDetails_appType,

    -- ** AppImageConfigDetails
    appImageConfigDetails_creationTime,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_lastModifiedTime,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_appImageConfigArn,

    -- ** AppSpecification
    appSpecification_containerArguments,
    appSpecification_containerEntrypoint,
    appSpecification_imageUri,

    -- ** ArtifactSource
    artifactSource_sourceTypes,
    artifactSource_sourceUri,

    -- ** ArtifactSourceType
    artifactSourceType_sourceIdType,
    artifactSourceType_value,

    -- ** ArtifactSummary
    artifactSummary_creationTime,
    artifactSummary_lastModifiedTime,
    artifactSummary_artifactName,
    artifactSummary_source,
    artifactSummary_artifactArn,
    artifactSummary_artifactType,

    -- ** AssociationSummary
    associationSummary_creationTime,
    associationSummary_sourceName,
    associationSummary_sourceType,
    associationSummary_sourceArn,
    associationSummary_createdBy,
    associationSummary_associationType,
    associationSummary_destinationArn,
    associationSummary_destinationType,
    associationSummary_destinationName,

    -- ** AsyncInferenceClientConfig
    asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance,

    -- ** AsyncInferenceConfig
    asyncInferenceConfig_clientConfig,
    asyncInferenceConfig_outputConfig,

    -- ** AsyncInferenceNotificationConfig
    asyncInferenceNotificationConfig_errorTopic,
    asyncInferenceNotificationConfig_successTopic,

    -- ** AsyncInferenceOutputConfig
    asyncInferenceOutputConfig_notificationConfig,
    asyncInferenceOutputConfig_kmsKeyId,
    asyncInferenceOutputConfig_s3OutputPath,

    -- ** AthenaDatasetDefinition
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- ** AutoMLCandidate
    autoMLCandidate_failureReason,
    autoMLCandidate_inferenceContainers,
    autoMLCandidate_candidateProperties,
    autoMLCandidate_endTime,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_candidateName,
    autoMLCandidate_objectiveStatus,
    autoMLCandidate_candidateSteps,
    autoMLCandidate_candidateStatus,
    autoMLCandidate_creationTime,
    autoMLCandidate_lastModifiedTime,

    -- ** AutoMLCandidateStep
    autoMLCandidateStep_candidateStepType,
    autoMLCandidateStep_candidateStepArn,
    autoMLCandidateStep_candidateStepName,

    -- ** AutoMLChannel
    autoMLChannel_compressionType,
    autoMLChannel_dataSource,
    autoMLChannel_targetAttributeName,

    -- ** AutoMLContainerDefinition
    autoMLContainerDefinition_environment,
    autoMLContainerDefinition_image,
    autoMLContainerDefinition_modelDataUrl,

    -- ** AutoMLDataSource
    autoMLDataSource_s3DataSource,

    -- ** AutoMLJobArtifacts
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,
    autoMLJobArtifacts_dataExplorationNotebookLocation,

    -- ** AutoMLJobCompletionCriteria
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,

    -- ** AutoMLJobConfig
    autoMLJobConfig_securityConfig,
    autoMLJobConfig_completionCriteria,

    -- ** AutoMLJobObjective
    autoMLJobObjective_metricName,

    -- ** AutoMLJobSummary
    autoMLJobSummary_failureReason,
    autoMLJobSummary_partialFailureReasons,
    autoMLJobSummary_endTime,
    autoMLJobSummary_autoMLJobName,
    autoMLJobSummary_autoMLJobArn,
    autoMLJobSummary_autoMLJobStatus,
    autoMLJobSummary_autoMLJobSecondaryStatus,
    autoMLJobSummary_creationTime,
    autoMLJobSummary_lastModifiedTime,

    -- ** AutoMLOutputDataConfig
    autoMLOutputDataConfig_kmsKeyId,
    autoMLOutputDataConfig_s3OutputPath,

    -- ** AutoMLPartialFailureReason
    autoMLPartialFailureReason_partialFailureMessage,

    -- ** AutoMLS3DataSource
    autoMLS3DataSource_s3DataType,
    autoMLS3DataSource_s3Uri,

    -- ** AutoMLSecurityConfig
    autoMLSecurityConfig_vpcConfig,
    autoMLSecurityConfig_volumeKmsKeyId,
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,

    -- ** AutoRollbackConfig
    autoRollbackConfig_alarms,

    -- ** Bias
    bias_report,

    -- ** BlueGreenUpdatePolicy
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- ** CacheHitResult
    cacheHitResult_sourcePipelineExecutionArn,

    -- ** CallbackStepMetadata
    callbackStepMetadata_callbackToken,
    callbackStepMetadata_outputParameters,
    callbackStepMetadata_sqsQueueUrl,

    -- ** CandidateArtifactLocations
    candidateArtifactLocations_explainability,

    -- ** CandidateProperties
    candidateProperties_candidateArtifactLocations,
    candidateProperties_candidateMetrics,

    -- ** CapacitySize
    capacitySize_type,
    capacitySize_value,

    -- ** CaptureContentTypeHeader
    captureContentTypeHeader_csvContentTypes,
    captureContentTypeHeader_jsonContentTypes,

    -- ** CaptureOption
    captureOption_captureMode,

    -- ** CategoricalParameterRange
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- ** CategoricalParameterRangeSpecification
    categoricalParameterRangeSpecification_values,

    -- ** Channel
    channel_shuffleConfig,
    channel_recordWrapperType,
    channel_inputMode,
    channel_compressionType,
    channel_contentType,
    channel_channelName,
    channel_dataSource,

    -- ** ChannelSpecification
    channelSpecification_supportedCompressionTypes,
    channelSpecification_isRequired,
    channelSpecification_description,
    channelSpecification_name,
    channelSpecification_supportedContentTypes,
    channelSpecification_supportedInputModes,

    -- ** CheckpointConfig
    checkpointConfig_localPath,
    checkpointConfig_s3Uri,

    -- ** CodeRepositorySummary
    codeRepositorySummary_gitConfig,
    codeRepositorySummary_codeRepositoryName,
    codeRepositorySummary_codeRepositoryArn,
    codeRepositorySummary_creationTime,
    codeRepositorySummary_lastModifiedTime,

    -- ** CognitoConfig
    cognitoConfig_userPool,
    cognitoConfig_clientId,

    -- ** CognitoMemberDefinition
    cognitoMemberDefinition_userPool,
    cognitoMemberDefinition_userGroup,
    cognitoMemberDefinition_clientId,

    -- ** CollectionConfiguration
    collectionConfiguration_collectionParameters,
    collectionConfiguration_collectionName,

    -- ** CompilationJobSummary
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationEndTime,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_compilationJobName,
    compilationJobSummary_compilationJobArn,
    compilationJobSummary_creationTime,
    compilationJobSummary_compilationJobStatus,

    -- ** ConditionStepMetadata
    conditionStepMetadata_outcome,

    -- ** ContainerDefinition
    containerDefinition_multiModelConfig,
    containerDefinition_modelDataUrl,
    containerDefinition_image,
    containerDefinition_modelPackageName,
    containerDefinition_environment,
    containerDefinition_imageConfig,
    containerDefinition_mode,
    containerDefinition_containerHostname,

    -- ** ContextSource
    contextSource_sourceType,
    contextSource_sourceId,
    contextSource_sourceUri,

    -- ** ContextSummary
    contextSummary_creationTime,
    contextSummary_lastModifiedTime,
    contextSummary_contextType,
    contextSummary_contextArn,
    contextSummary_source,
    contextSummary_contextName,

    -- ** ContinuousParameterRange
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_minValue,
    continuousParameterRange_maxValue,

    -- ** ContinuousParameterRangeSpecification
    continuousParameterRangeSpecification_minValue,
    continuousParameterRangeSpecification_maxValue,

    -- ** CustomImage
    customImage_imageVersionNumber,
    customImage_imageName,
    customImage_appImageConfigName,

    -- ** DataCaptureConfig
    dataCaptureConfig_captureContentTypeHeader,
    dataCaptureConfig_kmsKeyId,
    dataCaptureConfig_enableCapture,
    dataCaptureConfig_initialSamplingPercentage,
    dataCaptureConfig_destinationS3Uri,
    dataCaptureConfig_captureOptions,

    -- ** DataCaptureConfigSummary
    dataCaptureConfigSummary_enableCapture,
    dataCaptureConfigSummary_captureStatus,
    dataCaptureConfigSummary_currentSamplingPercentage,
    dataCaptureConfigSummary_destinationS3Uri,
    dataCaptureConfigSummary_kmsKeyId,

    -- ** DataCatalogConfig
    dataCatalogConfig_tableName,
    dataCatalogConfig_catalog,
    dataCatalogConfig_database,

    -- ** DataProcessing
    dataProcessing_outputFilter,
    dataProcessing_joinSource,
    dataProcessing_inputFilter,

    -- ** DataQualityAppSpecification
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- ** DataQualityBaselineConfig
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_statisticsResource,
    dataQualityBaselineConfig_baseliningJobName,

    -- ** DataQualityJobInput
    dataQualityJobInput_endpointInput,

    -- ** DataSource
    dataSource_s3DataSource,
    dataSource_fileSystemDataSource,

    -- ** DatasetDefinition
    datasetDefinition_redshiftDatasetDefinition,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_localPath,
    datasetDefinition_dataDistributionType,
    datasetDefinition_inputMode,

    -- ** DebugHookConfig
    debugHookConfig_localPath,
    debugHookConfig_collectionConfigurations,
    debugHookConfig_hookParameters,
    debugHookConfig_s3OutputPath,

    -- ** DebugRuleConfiguration
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- ** DebugRuleEvaluationStatus
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_statusDetails,
    debugRuleEvaluationStatus_ruleEvaluationStatus,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleConfigurationName,

    -- ** DeployedImage
    deployedImage_resolvedImage,
    deployedImage_specifiedImage,
    deployedImage_resolutionTime,

    -- ** DeploymentConfig
    deploymentConfig_autoRollbackConfiguration,
    deploymentConfig_blueGreenUpdatePolicy,

    -- ** DesiredWeightAndCapacity
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_variantName,

    -- ** Device
    device_description,
    device_iotThingName,
    device_deviceName,

    -- ** DeviceFleetSummary
    deviceFleetSummary_creationTime,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- ** DeviceStats
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- ** DeviceSummary
    deviceSummary_registrationTime,
    deviceSummary_models,
    deviceSummary_latestHeartbeat,
    deviceSummary_description,
    deviceSummary_deviceFleetName,
    deviceSummary_iotThingName,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- ** DomainDetails
    domainDetails_creationTime,
    domainDetails_status,
    domainDetails_domainArn,
    domainDetails_url,
    domainDetails_lastModifiedTime,
    domainDetails_domainName,
    domainDetails_domainId,

    -- ** EdgeModel
    edgeModel_latestInference,
    edgeModel_latestSampleTime,
    edgeModel_modelName,
    edgeModel_modelVersion,

    -- ** EdgeModelStat
    edgeModelStat_modelName,
    edgeModelStat_modelVersion,
    edgeModelStat_offlineDeviceCount,
    edgeModelStat_connectedDeviceCount,
    edgeModelStat_activeDeviceCount,
    edgeModelStat_samplingDeviceCount,

    -- ** EdgeModelSummary
    edgeModelSummary_modelName,
    edgeModelSummary_modelVersion,

    -- ** EdgeOutputConfig
    edgeOutputConfig_presetDeploymentType,
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_s3OutputLocation,

    -- ** EdgePackagingJobSummary
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- ** EdgePresetDeploymentOutput
    edgePresetDeploymentOutput_status,
    edgePresetDeploymentOutput_artifact,
    edgePresetDeploymentOutput_statusMessage,
    edgePresetDeploymentOutput_type,

    -- ** Endpoint
    endpoint_failureReason,
    endpoint_productionVariants,
    endpoint_monitoringSchedules,
    endpoint_dataCaptureConfig,
    endpoint_tags,
    endpoint_endpointName,
    endpoint_endpointArn,
    endpoint_endpointConfigName,
    endpoint_endpointStatus,
    endpoint_creationTime,
    endpoint_lastModifiedTime,

    -- ** EndpointConfigSummary
    endpointConfigSummary_endpointConfigName,
    endpointConfigSummary_endpointConfigArn,
    endpointConfigSummary_creationTime,

    -- ** EndpointInput
    endpointInput_inferenceAttribute,
    endpointInput_s3DataDistributionType,
    endpointInput_s3InputMode,
    endpointInput_startTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_endTimeOffset,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_probabilityAttribute,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- ** EndpointSummary
    endpointSummary_endpointName,
    endpointSummary_endpointArn,
    endpointSummary_creationTime,
    endpointSummary_lastModifiedTime,
    endpointSummary_endpointStatus,

    -- ** Experiment
    experiment_creationTime,
    experiment_createdBy,
    experiment_lastModifiedTime,
    experiment_experimentName,
    experiment_experimentArn,
    experiment_source,
    experiment_displayName,
    experiment_lastModifiedBy,
    experiment_description,
    experiment_tags,

    -- ** ExperimentConfig
    experimentConfig_trialComponentDisplayName,
    experimentConfig_experimentName,
    experimentConfig_trialName,

    -- ** ExperimentSource
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- ** ExperimentSummary
    experimentSummary_creationTime,
    experimentSummary_lastModifiedTime,
    experimentSummary_experimentName,
    experimentSummary_experimentSource,
    experimentSummary_experimentArn,
    experimentSummary_displayName,

    -- ** Explainability
    explainability_report,

    -- ** FeatureDefinition
    featureDefinition_featureType,
    featureDefinition_featureName,

    -- ** FeatureGroup
    featureGroup_creationTime,
    featureGroup_offlineStoreConfig,
    featureGroup_failureReason,
    featureGroup_featureGroupStatus,
    featureGroup_featureDefinitions,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_eventTimeFeatureName,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_featureGroupArn,
    featureGroup_featureGroupName,
    featureGroup_description,
    featureGroup_tags,
    featureGroup_roleArn,

    -- ** FeatureGroupSummary
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- ** FileSystemConfig
    fileSystemConfig_defaultGid,
    fileSystemConfig_mountPath,
    fileSystemConfig_defaultUid,

    -- ** FileSystemDataSource
    fileSystemDataSource_fileSystemId,
    fileSystemDataSource_fileSystemAccessMode,
    fileSystemDataSource_fileSystemType,
    fileSystemDataSource_directoryPath,

    -- ** Filter
    filter_operator,
    filter_value,
    filter_name,

    -- ** FinalAutoMLJobObjectiveMetric
    finalAutoMLJobObjectiveMetric_type,
    finalAutoMLJobObjectiveMetric_metricName,
    finalAutoMLJobObjectiveMetric_value,

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    finalHyperParameterTuningJobObjectiveMetric_type,
    finalHyperParameterTuningJobObjectiveMetric_metricName,
    finalHyperParameterTuningJobObjectiveMetric_value,

    -- ** FlowDefinitionOutputConfig
    flowDefinitionOutputConfig_kmsKeyId,
    flowDefinitionOutputConfig_s3OutputPath,

    -- ** FlowDefinitionSummary
    flowDefinitionSummary_failureReason,
    flowDefinitionSummary_flowDefinitionName,
    flowDefinitionSummary_flowDefinitionArn,
    flowDefinitionSummary_flowDefinitionStatus,
    flowDefinitionSummary_creationTime,

    -- ** GitConfig
    gitConfig_branch,
    gitConfig_secretArn,
    gitConfig_repositoryUrl,

    -- ** GitConfigForUpdate
    gitConfigForUpdate_secretArn,

    -- ** HumanLoopActivationConditionsConfig
    humanLoopActivationConditionsConfig_humanLoopActivationConditions,

    -- ** HumanLoopActivationConfig
    humanLoopActivationConfig_humanLoopActivationConditionsConfig,

    -- ** HumanLoopConfig
    humanLoopConfig_taskKeywords,
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_taskTimeLimitInSeconds,
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_workteamArn,
    humanLoopConfig_humanTaskUiArn,
    humanLoopConfig_taskTitle,
    humanLoopConfig_taskDescription,
    humanLoopConfig_taskCount,

    -- ** HumanLoopRequestSource
    humanLoopRequestSource_awsManagedHumanLoopRequestSource,

    -- ** HumanTaskConfig
    humanTaskConfig_taskKeywords,
    humanTaskConfig_publicWorkforceTaskPrice,
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
    humanTaskConfig_maxConcurrentTaskCount,
    humanTaskConfig_workteamArn,
    humanTaskConfig_uiConfig,
    humanTaskConfig_preHumanTaskLambdaArn,
    humanTaskConfig_taskTitle,
    humanTaskConfig_taskDescription,
    humanTaskConfig_numberOfHumanWorkersPerDataObject,
    humanTaskConfig_taskTimeLimitInSeconds,
    humanTaskConfig_annotationConsolidationConfig,

    -- ** HumanTaskUiSummary
    humanTaskUiSummary_humanTaskUiName,
    humanTaskUiSummary_humanTaskUiArn,
    humanTaskUiSummary_creationTime,

    -- ** HyperParameterAlgorithmSpecification
    hyperParameterAlgorithmSpecification_algorithmName,
    hyperParameterAlgorithmSpecification_trainingImage,
    hyperParameterAlgorithmSpecification_metricDefinitions,
    hyperParameterAlgorithmSpecification_trainingInputMode,

    -- ** HyperParameterSpecification
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_defaultValue,
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_description,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- ** HyperParameterTrainingJobDefinition
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- ** HyperParameterTrainingJobSummary
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_tuningJobName,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_trainingJobName,
    hyperParameterTrainingJobSummary_trainingJobArn,
    hyperParameterTrainingJobSummary_creationTime,
    hyperParameterTrainingJobSummary_trainingJobStatus,
    hyperParameterTrainingJobSummary_tunedHyperParameters,

    -- ** HyperParameterTuningJobConfig
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- ** HyperParameterTuningJobObjective
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- ** HyperParameterTuningJobSummary
    hyperParameterTuningJobSummary_resourceLimits,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
    hyperParameterTuningJobSummary_hyperParameterTuningJobName,
    hyperParameterTuningJobSummary_hyperParameterTuningJobArn,
    hyperParameterTuningJobSummary_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSummary_strategy,
    hyperParameterTuningJobSummary_creationTime,
    hyperParameterTuningJobSummary_trainingJobStatusCounters,
    hyperParameterTuningJobSummary_objectiveStatusCounters,

    -- ** HyperParameterTuningJobWarmStartConfig
    hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs,
    hyperParameterTuningJobWarmStartConfig_warmStartType,

    -- ** Image
    image_failureReason,
    image_displayName,
    image_description,
    image_creationTime,
    image_imageArn,
    image_imageName,
    image_imageStatus,
    image_lastModifiedTime,

    -- ** ImageConfig
    imageConfig_repositoryAuthConfig,
    imageConfig_repositoryAccessMode,

    -- ** ImageVersion
    imageVersion_failureReason,
    imageVersion_creationTime,
    imageVersion_imageArn,
    imageVersion_imageVersionArn,
    imageVersion_imageVersionStatus,
    imageVersion_lastModifiedTime,
    imageVersion_version,

    -- ** InferenceExecutionConfig
    inferenceExecutionConfig_mode,

    -- ** InferenceSpecification
    inferenceSpecification_supportedRealtimeInferenceInstanceTypes,
    inferenceSpecification_supportedTransformInstanceTypes,
    inferenceSpecification_containers,
    inferenceSpecification_supportedContentTypes,
    inferenceSpecification_supportedResponseMIMETypes,

    -- ** InputConfig
    inputConfig_frameworkVersion,
    inputConfig_s3Uri,
    inputConfig_dataInputConfig,
    inputConfig_framework,

    -- ** IntegerParameterRange
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_minValue,
    integerParameterRange_maxValue,

    -- ** IntegerParameterRangeSpecification
    integerParameterRangeSpecification_minValue,
    integerParameterRangeSpecification_maxValue,

    -- ** JupyterServerAppSettings
    jupyterServerAppSettings_defaultResourceSpec,
    jupyterServerAppSettings_lifecycleConfigArns,

    -- ** KernelGatewayAppSettings
    kernelGatewayAppSettings_defaultResourceSpec,
    kernelGatewayAppSettings_customImages,
    kernelGatewayAppSettings_lifecycleConfigArns,

    -- ** KernelGatewayImageConfig
    kernelGatewayImageConfig_fileSystemConfig,
    kernelGatewayImageConfig_kernelSpecs,

    -- ** KernelSpec
    kernelSpec_displayName,
    kernelSpec_name,

    -- ** LabelCounters
    labelCounters_machineLabeled,
    labelCounters_totalLabeled,
    labelCounters_failedNonRetryableError,
    labelCounters_unlabeled,
    labelCounters_humanLabeled,

    -- ** LabelCountersForWorkteam
    labelCountersForWorkteam_pendingHuman,
    labelCountersForWorkteam_total,
    labelCountersForWorkteam_humanLabeled,

    -- ** LabelingJobAlgorithmsConfig
    labelingJobAlgorithmsConfig_labelingJobResourceConfig,
    labelingJobAlgorithmsConfig_initialActiveLearningModelArn,
    labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn,

    -- ** LabelingJobDataAttributes
    labelingJobDataAttributes_contentClassifiers,

    -- ** LabelingJobDataSource
    labelingJobDataSource_s3DataSource,
    labelingJobDataSource_snsDataSource,

    -- ** LabelingJobForWorkteamSummary
    labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject,
    labelingJobForWorkteamSummary_labelCounters,
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_jobReferenceCode,
    labelingJobForWorkteamSummary_workRequesterAccountId,
    labelingJobForWorkteamSummary_creationTime,

    -- ** LabelingJobInputConfig
    labelingJobInputConfig_dataAttributes,
    labelingJobInputConfig_dataSource,

    -- ** LabelingJobOutput
    labelingJobOutput_finalActiveLearningModelArn,
    labelingJobOutput_outputDatasetS3Uri,

    -- ** LabelingJobOutputConfig
    labelingJobOutputConfig_snsTopicArn,
    labelingJobOutputConfig_kmsKeyId,
    labelingJobOutputConfig_s3OutputPath,

    -- ** LabelingJobResourceConfig
    labelingJobResourceConfig_volumeKmsKeyId,

    -- ** LabelingJobS3DataSource
    labelingJobS3DataSource_manifestS3Uri,

    -- ** LabelingJobSnsDataSource
    labelingJobSnsDataSource_snsTopicArn,

    -- ** LabelingJobStoppingConditions
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,

    -- ** LabelingJobSummary
    labelingJobSummary_failureReason,
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_inputConfig,
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_labelingJobName,
    labelingJobSummary_labelingJobArn,
    labelingJobSummary_creationTime,
    labelingJobSummary_lastModifiedTime,
    labelingJobSummary_labelingJobStatus,
    labelingJobSummary_labelCounters,
    labelingJobSummary_workteamArn,
    labelingJobSummary_preHumanTaskLambdaArn,

    -- ** LambdaStepMetadata
    lambdaStepMetadata_arn,
    lambdaStepMetadata_outputParameters,

    -- ** MemberDefinition
    memberDefinition_oidcMemberDefinition,
    memberDefinition_cognitoMemberDefinition,

    -- ** MetadataProperties
    metadataProperties_commitId,
    metadataProperties_repository,
    metadataProperties_generatedBy,
    metadataProperties_projectId,

    -- ** MetricData
    metricData_metricName,
    metricData_value,
    metricData_timestamp,

    -- ** MetricDatum
    metricDatum_set,
    metricDatum_metricName,
    metricDatum_value,

    -- ** MetricDefinition
    metricDefinition_name,
    metricDefinition_regex,

    -- ** MetricsSource
    metricsSource_contentDigest,
    metricsSource_contentType,
    metricsSource_s3Uri,

    -- ** ModelArtifacts
    modelArtifacts_s3ModelArtifacts,

    -- ** ModelBiasAppSpecification
    modelBiasAppSpecification_environment,
    modelBiasAppSpecification_imageUri,
    modelBiasAppSpecification_configUri,

    -- ** ModelBiasBaselineConfig
    modelBiasBaselineConfig_constraintsResource,
    modelBiasBaselineConfig_baseliningJobName,

    -- ** ModelBiasJobInput
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_groundTruthS3Input,

    -- ** ModelClientConfig
    modelClientConfig_invocationsTimeoutInSeconds,
    modelClientConfig_invocationsMaxRetries,

    -- ** ModelDataQuality
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- ** ModelDeployConfig
    modelDeployConfig_endpointName,
    modelDeployConfig_autoGenerateEndpointName,

    -- ** ModelDeployResult
    modelDeployResult_endpointName,

    -- ** ModelDigests
    modelDigests_artifactDigest,

    -- ** ModelExplainabilityAppSpecification
    modelExplainabilityAppSpecification_environment,
    modelExplainabilityAppSpecification_imageUri,
    modelExplainabilityAppSpecification_configUri,

    -- ** ModelExplainabilityBaselineConfig
    modelExplainabilityBaselineConfig_constraintsResource,
    modelExplainabilityBaselineConfig_baseliningJobName,

    -- ** ModelExplainabilityJobInput
    modelExplainabilityJobInput_endpointInput,

    -- ** ModelMetrics
    modelMetrics_bias,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,
    modelMetrics_explainability,

    -- ** ModelPackage
    modelPackage_creationTime,
    modelPackage_metadataProperties,
    modelPackage_modelApprovalStatus,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_modelPackageName,
    modelPackage_modelPackageArn,
    modelPackage_modelMetrics,
    modelPackage_modelPackageDescription,
    modelPackage_createdBy,
    modelPackage_lastModifiedTime,
    modelPackage_validationSpecification,
    modelPackage_inferenceSpecification,
    modelPackage_approvalDescription,
    modelPackage_modelPackageVersion,
    modelPackage_certifyForMarketplace,
    modelPackage_modelPackageGroupName,
    modelPackage_lastModifiedBy,
    modelPackage_modelPackageStatusDetails,
    modelPackage_tags,
    modelPackage_modelPackageStatus,

    -- ** ModelPackageContainerDefinition
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_image,

    -- ** ModelPackageGroup
    modelPackageGroup_creationTime,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_createdBy,
    modelPackageGroup_modelPackageGroupName,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_tags,

    -- ** ModelPackageGroupSummary
    modelPackageGroupSummary_modelPackageGroupDescription,
    modelPackageGroupSummary_modelPackageGroupName,
    modelPackageGroupSummary_modelPackageGroupArn,
    modelPackageGroupSummary_creationTime,
    modelPackageGroupSummary_modelPackageGroupStatus,

    -- ** ModelPackageStatusDetails
    modelPackageStatusDetails_imageScanStatuses,
    modelPackageStatusDetails_validationStatuses,

    -- ** ModelPackageStatusItem
    modelPackageStatusItem_failureReason,
    modelPackageStatusItem_name,
    modelPackageStatusItem_status,

    -- ** ModelPackageSummary
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
    modelPackageSummary_modelPackageVersion,
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelPackageName,
    modelPackageSummary_modelPackageArn,
    modelPackageSummary_creationTime,
    modelPackageSummary_modelPackageStatus,

    -- ** ModelPackageValidationProfile
    modelPackageValidationProfile_profileName,
    modelPackageValidationProfile_transformJobDefinition,

    -- ** ModelPackageValidationSpecification
    modelPackageValidationSpecification_validationRole,
    modelPackageValidationSpecification_validationProfiles,

    -- ** ModelQuality
    modelQuality_constraints,
    modelQuality_statistics,

    -- ** ModelQualityAppSpecification
    modelQualityAppSpecification_containerArguments,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_imageUri,

    -- ** ModelQualityBaselineConfig
    modelQualityBaselineConfig_constraintsResource,
    modelQualityBaselineConfig_baseliningJobName,

    -- ** ModelQualityJobInput
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_groundTruthS3Input,

    -- ** ModelStepMetadata
    modelStepMetadata_arn,

    -- ** ModelSummary
    modelSummary_modelName,
    modelSummary_modelArn,
    modelSummary_creationTime,

    -- ** MonitoringAppSpecification
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_recordPreprocessorSourceUri,
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_imageUri,

    -- ** MonitoringBaselineConfig
    monitoringBaselineConfig_constraintsResource,
    monitoringBaselineConfig_statisticsResource,
    monitoringBaselineConfig_baseliningJobName,

    -- ** MonitoringClusterConfig
    monitoringClusterConfig_volumeKmsKeyId,
    monitoringClusterConfig_instanceCount,
    monitoringClusterConfig_instanceType,
    monitoringClusterConfig_volumeSizeInGB,

    -- ** MonitoringConstraintsResource
    monitoringConstraintsResource_s3Uri,

    -- ** MonitoringExecutionSummary
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_monitoringScheduleName,
    monitoringExecutionSummary_scheduledTime,
    monitoringExecutionSummary_creationTime,
    monitoringExecutionSummary_lastModifiedTime,
    monitoringExecutionSummary_monitoringExecutionStatus,

    -- ** MonitoringGroundTruthS3Input
    monitoringGroundTruthS3Input_s3Uri,

    -- ** MonitoringInput
    monitoringInput_endpointInput,

    -- ** MonitoringJobDefinition
    monitoringJobDefinition_environment,
    monitoringJobDefinition_stoppingCondition,
    monitoringJobDefinition_networkConfig,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_monitoringInputs,
    monitoringJobDefinition_monitoringOutputConfig,
    monitoringJobDefinition_monitoringResources,
    monitoringJobDefinition_monitoringAppSpecification,
    monitoringJobDefinition_roleArn,

    -- ** MonitoringJobDefinitionSummary
    monitoringJobDefinitionSummary_monitoringJobDefinitionName,
    monitoringJobDefinitionSummary_monitoringJobDefinitionArn,
    monitoringJobDefinitionSummary_creationTime,
    monitoringJobDefinitionSummary_endpointName,

    -- ** MonitoringNetworkConfig
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_vpcConfig,
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,

    -- ** MonitoringOutput
    monitoringOutput_s3Output,

    -- ** MonitoringOutputConfig
    monitoringOutputConfig_kmsKeyId,
    monitoringOutputConfig_monitoringOutputs,

    -- ** MonitoringResources
    monitoringResources_clusterConfig,

    -- ** MonitoringS3Output
    monitoringS3Output_s3UploadMode,
    monitoringS3Output_s3Uri,
    monitoringS3Output_localPath,

    -- ** MonitoringSchedule
    monitoringSchedule_creationTime,
    monitoringSchedule_monitoringType,
    monitoringSchedule_failureReason,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_endpointName,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_tags,

    -- ** MonitoringScheduleConfig
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_scheduleConfig,
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringJobDefinitionName,

    -- ** MonitoringScheduleSummary
    monitoringScheduleSummary_monitoringType,
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringJobDefinitionName,
    monitoringScheduleSummary_monitoringScheduleName,
    monitoringScheduleSummary_monitoringScheduleArn,
    monitoringScheduleSummary_creationTime,
    monitoringScheduleSummary_lastModifiedTime,
    monitoringScheduleSummary_monitoringScheduleStatus,

    -- ** MonitoringStatisticsResource
    monitoringStatisticsResource_s3Uri,

    -- ** MonitoringStoppingCondition
    monitoringStoppingCondition_maxRuntimeInSeconds,

    -- ** MultiModelConfig
    multiModelConfig_modelCacheSetting,

    -- ** NeoVpcConfig
    neoVpcConfig_securityGroupIds,
    neoVpcConfig_subnets,

    -- ** NestedFilters
    nestedFilters_nestedPropertyName,
    nestedFilters_filters,

    -- ** NetworkConfig
    networkConfig_enableNetworkIsolation,
    networkConfig_vpcConfig,
    networkConfig_enableInterContainerTrafficEncryption,

    -- ** NotebookInstanceLifecycleConfigSummary
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- ** NotebookInstanceLifecycleHook
    notebookInstanceLifecycleHook_content,

    -- ** NotebookInstanceSummary
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_url,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationTopicArn,

    -- ** ObjectiveStatusCounters
    objectiveStatusCounters_pending,
    objectiveStatusCounters_succeeded,
    objectiveStatusCounters_failed,

    -- ** OfflineStoreConfig
    offlineStoreConfig_disableGlueTableCreation,
    offlineStoreConfig_dataCatalogConfig,
    offlineStoreConfig_s3StorageConfig,

    -- ** OfflineStoreStatus
    offlineStoreStatus_blockedReason,
    offlineStoreStatus_status,

    -- ** OidcConfig
    oidcConfig_clientId,
    oidcConfig_clientSecret,
    oidcConfig_issuer,
    oidcConfig_authorizationEndpoint,
    oidcConfig_tokenEndpoint,
    oidcConfig_userInfoEndpoint,
    oidcConfig_logoutEndpoint,
    oidcConfig_jwksUri,

    -- ** OidcConfigForResponse
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_userInfoEndpoint,
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_tokenEndpoint,
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_logoutEndpoint,

    -- ** OidcMemberDefinition
    oidcMemberDefinition_groups,

    -- ** OnlineStoreConfig
    onlineStoreConfig_securityConfig,
    onlineStoreConfig_enableOnlineStore,

    -- ** OnlineStoreSecurityConfig
    onlineStoreSecurityConfig_kmsKeyId,

    -- ** OutputConfig
    outputConfig_targetPlatform,
    outputConfig_kmsKeyId,
    outputConfig_compilerOptions,
    outputConfig_targetDevice,
    outputConfig_s3OutputLocation,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3OutputPath,

    -- ** OutputParameter
    outputParameter_name,
    outputParameter_value,

    -- ** Parameter
    parameter_name,
    parameter_value,

    -- ** ParameterRange
    parameterRange_categoricalParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,
    parameterRange_continuousParameterRangeSpecification,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- ** Parent
    parent_experimentName,
    parent_trialName,

    -- ** ParentHyperParameterTuningJob
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** Pipeline
    pipeline_creationTime,
    pipeline_pipelineDisplayName,
    pipeline_pipelineName,
    pipeline_createdBy,
    pipeline_lastRunTime,
    pipeline_lastModifiedTime,
    pipeline_pipelineStatus,
    pipeline_pipelineDescription,
    pipeline_pipelineArn,
    pipeline_lastModifiedBy,
    pipeline_tags,
    pipeline_roleArn,

    -- ** PipelineExecution
    pipelineExecution_creationTime,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_failureReason,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_pipelineParameters,
    pipelineExecution_createdBy,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_pipelineExperimentConfig,

    -- ** PipelineExecutionStep
    pipelineExecutionStep_failureReason,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepStatus,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_cacheHitResult,

    -- ** PipelineExecutionStepMetadata
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_tuningJob,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_callback,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionDescription,

    -- ** PipelineExperimentConfig
    pipelineExperimentConfig_experimentName,
    pipelineExperimentConfig_trialName,

    -- ** PipelineSummary
    pipelineSummary_creationTime,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_pipelineName,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineDescription,
    pipelineSummary_pipelineArn,
    pipelineSummary_roleArn,

    -- ** ProcessingClusterConfig
    processingClusterConfig_volumeKmsKeyId,
    processingClusterConfig_instanceCount,
    processingClusterConfig_instanceType,
    processingClusterConfig_volumeSizeInGB,

    -- ** ProcessingFeatureStoreOutput
    processingFeatureStoreOutput_featureGroupName,

    -- ** ProcessingInput
    processingInput_datasetDefinition,
    processingInput_appManaged,
    processingInput_s3Input,
    processingInput_inputName,

    -- ** ProcessingJob
    processingJob_creationTime,
    processingJob_failureReason,
    processingJob_monitoringScheduleArn,
    processingJob_appSpecification,
    processingJob_processingResources,
    processingJob_environment,
    processingJob_processingJobName,
    processingJob_stoppingCondition,
    processingJob_experimentConfig,
    processingJob_lastModifiedTime,
    processingJob_processingInputs,
    processingJob_networkConfig,
    processingJob_autoMLJobArn,
    processingJob_trainingJobArn,
    processingJob_processingJobStatus,
    processingJob_exitMessage,
    processingJob_processingOutputConfig,
    processingJob_processingStartTime,
    processingJob_processingEndTime,
    processingJob_tags,
    processingJob_processingJobArn,
    processingJob_roleArn,

    -- ** ProcessingJobStepMetadata
    processingJobStepMetadata_arn,

    -- ** ProcessingJobSummary
    processingJobSummary_failureReason,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_exitMessage,
    processingJobSummary_processingEndTime,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- ** ProcessingOutput
    processingOutput_featureStoreOutput,
    processingOutput_s3Output,
    processingOutput_appManaged,
    processingOutput_outputName,

    -- ** ProcessingOutputConfig
    processingOutputConfig_kmsKeyId,
    processingOutputConfig_outputs,

    -- ** ProcessingResources
    processingResources_clusterConfig,

    -- ** ProcessingS3Input
    processingS3Input_s3DataDistributionType,
    processingS3Input_s3InputMode,
    processingS3Input_localPath,
    processingS3Input_s3CompressionType,
    processingS3Input_s3Uri,
    processingS3Input_s3DataType,

    -- ** ProcessingS3Output
    processingS3Output_s3Uri,
    processingS3Output_localPath,
    processingS3Output_s3UploadMode,

    -- ** ProcessingStoppingCondition
    processingStoppingCondition_maxRuntimeInSeconds,

    -- ** ProductionVariant
    productionVariant_acceleratorType,
    productionVariant_coreDumpConfig,
    productionVariant_initialVariantWeight,
    productionVariant_variantName,
    productionVariant_modelName,
    productionVariant_initialInstanceCount,
    productionVariant_instanceType,

    -- ** ProductionVariantCoreDumpConfig
    productionVariantCoreDumpConfig_kmsKeyId,
    productionVariantCoreDumpConfig_destinationS3Uri,

    -- ** ProductionVariantSummary
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_currentWeight,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_deployedImages,
    productionVariantSummary_variantName,

    -- ** ProfilerConfig
    profilerConfig_profilingParameters,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_s3OutputPath,

    -- ** ProfilerConfigForUpdate
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_s3OutputPath,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_disableProfiler,

    -- ** ProfilerRuleConfiguration
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- ** ProfilerRuleEvaluationStatus
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_statusDetails,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleConfigurationName,

    -- ** Project
    project_creationTime,
    project_serviceCatalogProvisionedProductDetails,
    project_createdBy,
    project_projectStatus,
    project_projectName,
    project_serviceCatalogProvisioningDetails,
    project_projectId,
    project_projectArn,
    project_projectDescription,
    project_tags,

    -- ** ProjectSummary
    projectSummary_projectDescription,
    projectSummary_projectName,
    projectSummary_projectArn,
    projectSummary_projectId,
    projectSummary_creationTime,
    projectSummary_projectStatus,

    -- ** PropertyNameQuery
    propertyNameQuery_propertyNameHint,

    -- ** PropertyNameSuggestion
    propertyNameSuggestion_propertyName,

    -- ** ProvisioningParameter
    provisioningParameter_value,
    provisioningParameter_key,

    -- ** PublicWorkforceTaskPrice
    publicWorkforceTaskPrice_amountInUsd,

    -- ** RedshiftDatasetDefinition
    redshiftDatasetDefinition_kmsKeyId,
    redshiftDatasetDefinition_outputCompression,
    redshiftDatasetDefinition_clusterId,
    redshiftDatasetDefinition_database,
    redshiftDatasetDefinition_dbUser,
    redshiftDatasetDefinition_queryString,
    redshiftDatasetDefinition_clusterRoleArn,
    redshiftDatasetDefinition_outputS3Uri,
    redshiftDatasetDefinition_outputFormat,

    -- ** RegisterModelStepMetadata
    registerModelStepMetadata_arn,

    -- ** RenderableTask
    renderableTask_input,

    -- ** RenderingError
    renderingError_code,
    renderingError_message,

    -- ** RepositoryAuthConfig
    repositoryAuthConfig_repositoryCredentialsProviderArn,

    -- ** ResolvedAttributes
    resolvedAttributes_problemType,
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_completionCriteria,

    -- ** ResourceConfig
    resourceConfig_volumeKmsKeyId,
    resourceConfig_instanceType,
    resourceConfig_instanceCount,
    resourceConfig_volumeSizeInGB,

    -- ** ResourceLimits
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- ** ResourceSpec
    resourceSpec_instanceType,
    resourceSpec_sageMakerImageArn,
    resourceSpec_sageMakerImageVersionArn,
    resourceSpec_lifecycleConfigArn,

    -- ** RetentionPolicy
    retentionPolicy_homeEfsFileSystem,

    -- ** RetryStrategy
    retryStrategy_maximumRetryAttempts,

    -- ** S3DataSource
    s3DataSource_s3DataDistributionType,
    s3DataSource_attributeNames,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- ** S3StorageConfig
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_s3Uri,

    -- ** ScheduleConfig
    scheduleConfig_scheduleExpression,

    -- ** SearchExpression
    searchExpression_subExpressions,
    searchExpression_operator,
    searchExpression_filters,
    searchExpression_nestedFilters,

    -- ** SearchRecord
    searchRecord_trainingJob,
    searchRecord_trial,
    searchRecord_modelPackageGroup,
    searchRecord_trialComponent,
    searchRecord_project,
    searchRecord_pipelineExecution,
    searchRecord_featureGroup,
    searchRecord_experiment,
    searchRecord_pipeline,
    searchRecord_modelPackage,
    searchRecord_endpoint,

    -- ** SecondaryStatusTransition
    secondaryStatusTransition_statusMessage,
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_status,
    secondaryStatusTransition_startTime,

    -- ** ServiceCatalogProvisionedProductDetails
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,
    serviceCatalogProvisionedProductDetails_provisionedProductId,

    -- ** ServiceCatalogProvisioningDetails
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_productId,

    -- ** SharingSettings
    sharingSettings_s3KmsKeyId,
    sharingSettings_s3OutputPath,
    sharingSettings_notebookOutputOption,

    -- ** ShuffleConfig
    shuffleConfig_seed,

    -- ** SourceAlgorithm
    sourceAlgorithm_modelDataUrl,
    sourceAlgorithm_algorithmName,

    -- ** SourceAlgorithmSpecification
    sourceAlgorithmSpecification_sourceAlgorithms,

    -- ** SourceIpConfig
    sourceIpConfig_cidrs,

    -- ** StoppingCondition
    stoppingCondition_maxWaitTimeInSeconds,
    stoppingCondition_maxRuntimeInSeconds,

    -- ** StudioLifecycleConfigDetails
    studioLifecycleConfigDetails_creationTime,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_studioLifecycleConfigName,

    -- ** SubscribedWorkteam
    subscribedWorkteam_marketplaceTitle,
    subscribedWorkteam_sellerName,
    subscribedWorkteam_listingId,
    subscribedWorkteam_marketplaceDescription,
    subscribedWorkteam_workteamArn,

    -- ** SuggestionQuery
    suggestionQuery_propertyNameQuery,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetPlatform
    targetPlatform_accelerator,
    targetPlatform_os,
    targetPlatform_arch,

    -- ** TensorBoardAppSettings
    tensorBoardAppSettings_defaultResourceSpec,

    -- ** TensorBoardOutputConfig
    tensorBoardOutputConfig_localPath,
    tensorBoardOutputConfig_s3OutputPath,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- ** TrainingJob
    trainingJob_creationTime,
    trainingJob_labelingJobArn,
    trainingJob_failureReason,
    trainingJob_secondaryStatusTransitions,
    trainingJob_modelArtifacts,
    trainingJob_trainingEndTime,
    trainingJob_environment,
    trainingJob_billableTimeInSeconds,
    trainingJob_debugHookConfig,
    trainingJob_checkpointConfig,
    trainingJob_retryStrategy,
    trainingJob_stoppingCondition,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_trainingJobStatus,
    trainingJob_enableNetworkIsolation,
    trainingJob_experimentConfig,
    trainingJob_lastModifiedTime,
    trainingJob_debugRuleConfigurations,
    trainingJob_enableManagedSpotTraining,
    trainingJob_autoMLJobArn,
    trainingJob_hyperParameters,
    trainingJob_inputDataConfig,
    trainingJob_vpcConfig,
    trainingJob_trainingJobArn,
    trainingJob_algorithmSpecification,
    trainingJob_finalMetricDataList,
    trainingJob_outputDataConfig,
    trainingJob_trainingStartTime,
    trainingJob_tuningJobArn,
    trainingJob_trainingJobName,
    trainingJob_resourceConfig,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_secondaryStatus,
    trainingJob_tags,
    trainingJob_trainingTimeInSeconds,
    trainingJob_roleArn,

    -- ** TrainingJobDefinition
    trainingJobDefinition_hyperParameters,
    trainingJobDefinition_trainingInputMode,
    trainingJobDefinition_inputDataConfig,
    trainingJobDefinition_outputDataConfig,
    trainingJobDefinition_resourceConfig,
    trainingJobDefinition_stoppingCondition,

    -- ** TrainingJobStatusCounters
    trainingJobStatusCounters_stopped,
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_inProgress,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_completed,

    -- ** TrainingJobStepMetadata
    trainingJobStepMetadata_arn,

    -- ** TrainingJobSummary
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- ** TrainingSpecification
    trainingSpecification_trainingImageDigest,
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_metricDefinitions,
    trainingSpecification_trainingImage,
    trainingSpecification_supportedTrainingInstanceTypes,
    trainingSpecification_trainingChannels,

    -- ** TransformDataSource
    transformDataSource_s3DataSource,

    -- ** TransformInput
    transformInput_splitType,
    transformInput_compressionType,
    transformInput_contentType,
    transformInput_dataSource,

    -- ** TransformJob
    transformJob_creationTime,
    transformJob_labelingJobArn,
    transformJob_transformJobName,
    transformJob_failureReason,
    transformJob_modelClientConfig,
    transformJob_batchStrategy,
    transformJob_maxPayloadInMB,
    transformJob_environment,
    transformJob_transformResources,
    transformJob_modelName,
    transformJob_experimentConfig,
    transformJob_transformEndTime,
    transformJob_transformStartTime,
    transformJob_autoMLJobArn,
    transformJob_transformJobStatus,
    transformJob_transformInput,
    transformJob_maxConcurrentTransforms,
    transformJob_transformOutput,
    transformJob_dataProcessing,
    transformJob_transformJobArn,
    transformJob_tags,

    -- ** TransformJobDefinition
    transformJobDefinition_batchStrategy,
    transformJobDefinition_maxPayloadInMB,
    transformJobDefinition_environment,
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_transformInput,
    transformJobDefinition_transformOutput,
    transformJobDefinition_transformResources,

    -- ** TransformJobStepMetadata
    transformJobStepMetadata_arn,

    -- ** TransformJobSummary
    transformJobSummary_failureReason,
    transformJobSummary_lastModifiedTime,
    transformJobSummary_transformEndTime,
    transformJobSummary_transformJobName,
    transformJobSummary_transformJobArn,
    transformJobSummary_creationTime,
    transformJobSummary_transformJobStatus,

    -- ** TransformOutput
    transformOutput_assembleWith,
    transformOutput_accept,
    transformOutput_kmsKeyId,
    transformOutput_s3OutputPath,

    -- ** TransformResources
    transformResources_volumeKmsKeyId,
    transformResources_instanceType,
    transformResources_instanceCount,

    -- ** TransformS3DataSource
    transformS3DataSource_s3DataType,
    transformS3DataSource_s3Uri,

    -- ** Trial
    trial_creationTime,
    trial_metadataProperties,
    trial_trialComponentSummaries,
    trial_trialArn,
    trial_createdBy,
    trial_lastModifiedTime,
    trial_experimentName,
    trial_source,
    trial_displayName,
    trial_trialName,
    trial_lastModifiedBy,
    trial_tags,

    -- ** TrialComponent
    trialComponent_creationTime,
    trialComponent_metadataProperties,
    trialComponent_status,
    trialComponent_sourceDetail,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_startTime,
    trialComponent_createdBy,
    trialComponent_lastModifiedTime,
    trialComponent_parents,
    trialComponent_endTime,
    trialComponent_trialComponentName,
    trialComponent_parameters,
    trialComponent_source,
    trialComponent_displayName,
    trialComponent_lastModifiedBy,
    trialComponent_trialComponentArn,
    trialComponent_inputArtifacts,
    trialComponent_tags,

    -- ** TrialComponentArtifact
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- ** TrialComponentMetricSummary
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_stdDev,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_timeStamp,

    -- ** TrialComponentParameterValue
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- ** TrialComponentSimpleSummary
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_createdBy,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentSource,

    -- ** TrialComponentSource
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- ** TrialComponentSourceDetail
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_sourceArn,
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_transformJob,

    -- ** TrialComponentStatus
    trialComponentStatus_primaryStatus,
    trialComponentStatus_message,

    -- ** TrialComponentSummary
    trialComponentSummary_creationTime,
    trialComponentSummary_status,
    trialComponentSummary_startTime,
    trialComponentSummary_createdBy,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_endTime,
    trialComponentSummary_trialComponentName,
    trialComponentSummary_displayName,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentSource,

    -- ** TrialSource
    trialSource_sourceType,
    trialSource_sourceArn,

    -- ** TrialSummary
    trialSummary_creationTime,
    trialSummary_trialArn,
    trialSummary_lastModifiedTime,
    trialSummary_trialSource,
    trialSummary_displayName,
    trialSummary_trialName,

    -- ** TuningJobCompletionCriteria
    tuningJobCompletionCriteria_targetObjectiveMetricValue,

    -- ** TuningJobStepMetaData
    tuningJobStepMetaData_arn,

    -- ** USD
    usd_cents,
    usd_dollars,
    usd_tenthFractionsOfACent,

    -- ** UiConfig
    uiConfig_uiTemplateS3Uri,
    uiConfig_humanTaskUiArn,

    -- ** UiTemplate
    uiTemplate_content,

    -- ** UiTemplateInfo
    uiTemplateInfo_url,
    uiTemplateInfo_contentSha256,

    -- ** UserContext
    userContext_userProfileName,
    userContext_userProfileArn,
    userContext_domainId,

    -- ** UserProfileDetails
    userProfileDetails_creationTime,
    userProfileDetails_status,
    userProfileDetails_userProfileName,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_domainId,

    -- ** UserSettings
    userSettings_tensorBoardAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_securityGroups,
    userSettings_jupyterServerAppSettings,
    userSettings_sharingSettings,
    userSettings_executionRole,

    -- ** VariantProperty
    variantProperty_variantPropertyType,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- ** Workforce
    workforce_subDomain,
    workforce_createDate,
    workforce_sourceIpConfig,
    workforce_cognitoConfig,
    workforce_lastUpdatedDate,
    workforce_oidcConfig,
    workforce_workforceName,
    workforce_workforceArn,

    -- ** Workteam
    workteam_subDomain,
    workteam_productListingIds,
    workteam_notificationConfiguration,
    workteam_createDate,
    workteam_workforceArn,
    workteam_lastUpdatedDate,
    workteam_workteamName,
    workteam_memberDefinitions,
    workteam_workteamArn,
    workteam_description,
  )
where

import Amazonka.SageMaker.AddAssociation
import Amazonka.SageMaker.AddTags
import Amazonka.SageMaker.AssociateTrialComponent
import Amazonka.SageMaker.CreateAction
import Amazonka.SageMaker.CreateAlgorithm
import Amazonka.SageMaker.CreateApp
import Amazonka.SageMaker.CreateAppImageConfig
import Amazonka.SageMaker.CreateArtifact
import Amazonka.SageMaker.CreateAutoMLJob
import Amazonka.SageMaker.CreateCodeRepository
import Amazonka.SageMaker.CreateCompilationJob
import Amazonka.SageMaker.CreateContext
import Amazonka.SageMaker.CreateDataQualityJobDefinition
import Amazonka.SageMaker.CreateDeviceFleet
import Amazonka.SageMaker.CreateDomain
import Amazonka.SageMaker.CreateEdgePackagingJob
import Amazonka.SageMaker.CreateEndpoint
import Amazonka.SageMaker.CreateEndpointConfig
import Amazonka.SageMaker.CreateExperiment
import Amazonka.SageMaker.CreateFeatureGroup
import Amazonka.SageMaker.CreateFlowDefinition
import Amazonka.SageMaker.CreateHumanTaskUi
import Amazonka.SageMaker.CreateHyperParameterTuningJob
import Amazonka.SageMaker.CreateImage
import Amazonka.SageMaker.CreateImageVersion
import Amazonka.SageMaker.CreateLabelingJob
import Amazonka.SageMaker.CreateModel
import Amazonka.SageMaker.CreateModelBiasJobDefinition
import Amazonka.SageMaker.CreateModelExplainabilityJobDefinition
import Amazonka.SageMaker.CreateModelPackage
import Amazonka.SageMaker.CreateModelPackageGroup
import Amazonka.SageMaker.CreateModelQualityJobDefinition
import Amazonka.SageMaker.CreateMonitoringSchedule
import Amazonka.SageMaker.CreateNotebookInstance
import Amazonka.SageMaker.CreateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.CreatePipeline
import Amazonka.SageMaker.CreatePresignedDomainUrl
import Amazonka.SageMaker.CreatePresignedNotebookInstanceUrl
import Amazonka.SageMaker.CreateProcessingJob
import Amazonka.SageMaker.CreateProject
import Amazonka.SageMaker.CreateStudioLifecycleConfig
import Amazonka.SageMaker.CreateTrainingJob
import Amazonka.SageMaker.CreateTransformJob
import Amazonka.SageMaker.CreateTrial
import Amazonka.SageMaker.CreateTrialComponent
import Amazonka.SageMaker.CreateUserProfile
import Amazonka.SageMaker.CreateWorkforce
import Amazonka.SageMaker.CreateWorkteam
import Amazonka.SageMaker.DeleteAction
import Amazonka.SageMaker.DeleteAlgorithm
import Amazonka.SageMaker.DeleteApp
import Amazonka.SageMaker.DeleteAppImageConfig
import Amazonka.SageMaker.DeleteArtifact
import Amazonka.SageMaker.DeleteAssociation
import Amazonka.SageMaker.DeleteCodeRepository
import Amazonka.SageMaker.DeleteContext
import Amazonka.SageMaker.DeleteDataQualityJobDefinition
import Amazonka.SageMaker.DeleteDeviceFleet
import Amazonka.SageMaker.DeleteDomain
import Amazonka.SageMaker.DeleteEndpoint
import Amazonka.SageMaker.DeleteEndpointConfig
import Amazonka.SageMaker.DeleteExperiment
import Amazonka.SageMaker.DeleteFeatureGroup
import Amazonka.SageMaker.DeleteFlowDefinition
import Amazonka.SageMaker.DeleteHumanTaskUi
import Amazonka.SageMaker.DeleteImage
import Amazonka.SageMaker.DeleteImageVersion
import Amazonka.SageMaker.DeleteModel
import Amazonka.SageMaker.DeleteModelBiasJobDefinition
import Amazonka.SageMaker.DeleteModelExplainabilityJobDefinition
import Amazonka.SageMaker.DeleteModelPackage
import Amazonka.SageMaker.DeleteModelPackageGroup
import Amazonka.SageMaker.DeleteModelPackageGroupPolicy
import Amazonka.SageMaker.DeleteModelQualityJobDefinition
import Amazonka.SageMaker.DeleteMonitoringSchedule
import Amazonka.SageMaker.DeleteNotebookInstance
import Amazonka.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DeletePipeline
import Amazonka.SageMaker.DeleteProject
import Amazonka.SageMaker.DeleteStudioLifecycleConfig
import Amazonka.SageMaker.DeleteTags
import Amazonka.SageMaker.DeleteTrial
import Amazonka.SageMaker.DeleteTrialComponent
import Amazonka.SageMaker.DeleteUserProfile
import Amazonka.SageMaker.DeleteWorkforce
import Amazonka.SageMaker.DeleteWorkteam
import Amazonka.SageMaker.DeregisterDevices
import Amazonka.SageMaker.DescribeAction
import Amazonka.SageMaker.DescribeAlgorithm
import Amazonka.SageMaker.DescribeApp
import Amazonka.SageMaker.DescribeAppImageConfig
import Amazonka.SageMaker.DescribeArtifact
import Amazonka.SageMaker.DescribeAutoMLJob
import Amazonka.SageMaker.DescribeCodeRepository
import Amazonka.SageMaker.DescribeCompilationJob
import Amazonka.SageMaker.DescribeContext
import Amazonka.SageMaker.DescribeDataQualityJobDefinition
import Amazonka.SageMaker.DescribeDevice
import Amazonka.SageMaker.DescribeDeviceFleet
import Amazonka.SageMaker.DescribeDomain
import Amazonka.SageMaker.DescribeEdgePackagingJob
import Amazonka.SageMaker.DescribeEndpoint
import Amazonka.SageMaker.DescribeEndpointConfig
import Amazonka.SageMaker.DescribeExperiment
import Amazonka.SageMaker.DescribeFeatureGroup
import Amazonka.SageMaker.DescribeFlowDefinition
import Amazonka.SageMaker.DescribeHumanTaskUi
import Amazonka.SageMaker.DescribeHyperParameterTuningJob
import Amazonka.SageMaker.DescribeImage
import Amazonka.SageMaker.DescribeImageVersion
import Amazonka.SageMaker.DescribeLabelingJob
import Amazonka.SageMaker.DescribeModel
import Amazonka.SageMaker.DescribeModelBiasJobDefinition
import Amazonka.SageMaker.DescribeModelExplainabilityJobDefinition
import Amazonka.SageMaker.DescribeModelPackage
import Amazonka.SageMaker.DescribeModelPackageGroup
import Amazonka.SageMaker.DescribeModelQualityJobDefinition
import Amazonka.SageMaker.DescribeMonitoringSchedule
import Amazonka.SageMaker.DescribeNotebookInstance
import Amazonka.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DescribePipeline
import Amazonka.SageMaker.DescribePipelineDefinitionForExecution
import Amazonka.SageMaker.DescribePipelineExecution
import Amazonka.SageMaker.DescribeProcessingJob
import Amazonka.SageMaker.DescribeProject
import Amazonka.SageMaker.DescribeStudioLifecycleConfig
import Amazonka.SageMaker.DescribeSubscribedWorkteam
import Amazonka.SageMaker.DescribeTrainingJob
import Amazonka.SageMaker.DescribeTransformJob
import Amazonka.SageMaker.DescribeTrial
import Amazonka.SageMaker.DescribeTrialComponent
import Amazonka.SageMaker.DescribeUserProfile
import Amazonka.SageMaker.DescribeWorkforce
import Amazonka.SageMaker.DescribeWorkteam
import Amazonka.SageMaker.DisableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.DisassociateTrialComponent
import Amazonka.SageMaker.EnableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.GetDeviceFleetReport
import Amazonka.SageMaker.GetModelPackageGroupPolicy
import Amazonka.SageMaker.GetSagemakerServicecatalogPortfolioStatus
import Amazonka.SageMaker.GetSearchSuggestions
import Amazonka.SageMaker.ListActions
import Amazonka.SageMaker.ListAlgorithms
import Amazonka.SageMaker.ListAppImageConfigs
import Amazonka.SageMaker.ListApps
import Amazonka.SageMaker.ListArtifacts
import Amazonka.SageMaker.ListAssociations
import Amazonka.SageMaker.ListAutoMLJobs
import Amazonka.SageMaker.ListCandidatesForAutoMLJob
import Amazonka.SageMaker.ListCodeRepositories
import Amazonka.SageMaker.ListCompilationJobs
import Amazonka.SageMaker.ListContexts
import Amazonka.SageMaker.ListDataQualityJobDefinitions
import Amazonka.SageMaker.ListDeviceFleets
import Amazonka.SageMaker.ListDevices
import Amazonka.SageMaker.ListDomains
import Amazonka.SageMaker.ListEdgePackagingJobs
import Amazonka.SageMaker.ListEndpointConfigs
import Amazonka.SageMaker.ListEndpoints
import Amazonka.SageMaker.ListExperiments
import Amazonka.SageMaker.ListFeatureGroups
import Amazonka.SageMaker.ListFlowDefinitions
import Amazonka.SageMaker.ListHumanTaskUis
import Amazonka.SageMaker.ListHyperParameterTuningJobs
import Amazonka.SageMaker.ListImageVersions
import Amazonka.SageMaker.ListImages
import Amazonka.SageMaker.ListLabelingJobs
import Amazonka.SageMaker.ListLabelingJobsForWorkteam
import Amazonka.SageMaker.ListModelBiasJobDefinitions
import Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
import Amazonka.SageMaker.ListModelPackageGroups
import Amazonka.SageMaker.ListModelPackages
import Amazonka.SageMaker.ListModelQualityJobDefinitions
import Amazonka.SageMaker.ListModels
import Amazonka.SageMaker.ListMonitoringExecutions
import Amazonka.SageMaker.ListMonitoringSchedules
import Amazonka.SageMaker.ListNotebookInstanceLifecycleConfigs
import Amazonka.SageMaker.ListNotebookInstances
import Amazonka.SageMaker.ListPipelineExecutionSteps
import Amazonka.SageMaker.ListPipelineExecutions
import Amazonka.SageMaker.ListPipelineParametersForExecution
import Amazonka.SageMaker.ListPipelines
import Amazonka.SageMaker.ListProcessingJobs
import Amazonka.SageMaker.ListProjects
import Amazonka.SageMaker.ListStudioLifecycleConfigs
import Amazonka.SageMaker.ListSubscribedWorkteams
import Amazonka.SageMaker.ListTags
import Amazonka.SageMaker.ListTrainingJobs
import Amazonka.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Amazonka.SageMaker.ListTransformJobs
import Amazonka.SageMaker.ListTrialComponents
import Amazonka.SageMaker.ListTrials
import Amazonka.SageMaker.ListUserProfiles
import Amazonka.SageMaker.ListWorkforces
import Amazonka.SageMaker.ListWorkteams
import Amazonka.SageMaker.PutModelPackageGroupPolicy
import Amazonka.SageMaker.RegisterDevices
import Amazonka.SageMaker.RenderUiTemplate
import Amazonka.SageMaker.RetryPipelineExecution
import Amazonka.SageMaker.Search
import Amazonka.SageMaker.SendPipelineExecutionStepFailure
import Amazonka.SageMaker.SendPipelineExecutionStepSuccess
import Amazonka.SageMaker.StartMonitoringSchedule
import Amazonka.SageMaker.StartNotebookInstance
import Amazonka.SageMaker.StartPipelineExecution
import Amazonka.SageMaker.StopAutoMLJob
import Amazonka.SageMaker.StopCompilationJob
import Amazonka.SageMaker.StopEdgePackagingJob
import Amazonka.SageMaker.StopHyperParameterTuningJob
import Amazonka.SageMaker.StopLabelingJob
import Amazonka.SageMaker.StopMonitoringSchedule
import Amazonka.SageMaker.StopNotebookInstance
import Amazonka.SageMaker.StopPipelineExecution
import Amazonka.SageMaker.StopProcessingJob
import Amazonka.SageMaker.StopTrainingJob
import Amazonka.SageMaker.StopTransformJob
import Amazonka.SageMaker.Types.ActionSource
import Amazonka.SageMaker.Types.ActionSummary
import Amazonka.SageMaker.Types.AgentVersion
import Amazonka.SageMaker.Types.Alarm
import Amazonka.SageMaker.Types.AlgorithmSpecification
import Amazonka.SageMaker.Types.AlgorithmStatusDetails
import Amazonka.SageMaker.Types.AlgorithmStatusItem
import Amazonka.SageMaker.Types.AlgorithmSummary
import Amazonka.SageMaker.Types.AlgorithmValidationProfile
import Amazonka.SageMaker.Types.AlgorithmValidationSpecification
import Amazonka.SageMaker.Types.AnnotationConsolidationConfig
import Amazonka.SageMaker.Types.AppDetails
import Amazonka.SageMaker.Types.AppImageConfigDetails
import Amazonka.SageMaker.Types.AppSpecification
import Amazonka.SageMaker.Types.ArtifactSource
import Amazonka.SageMaker.Types.ArtifactSourceType
import Amazonka.SageMaker.Types.ArtifactSummary
import Amazonka.SageMaker.Types.AssociationSummary
import Amazonka.SageMaker.Types.AsyncInferenceClientConfig
import Amazonka.SageMaker.Types.AsyncInferenceConfig
import Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig
import Amazonka.SageMaker.Types.AsyncInferenceOutputConfig
import Amazonka.SageMaker.Types.AthenaDatasetDefinition
import Amazonka.SageMaker.Types.AutoMLCandidate
import Amazonka.SageMaker.Types.AutoMLCandidateStep
import Amazonka.SageMaker.Types.AutoMLChannel
import Amazonka.SageMaker.Types.AutoMLContainerDefinition
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.AutoMLJobArtifacts
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLJobConfig
import Amazonka.SageMaker.Types.AutoMLJobObjective
import Amazonka.SageMaker.Types.AutoMLJobSummary
import Amazonka.SageMaker.Types.AutoMLOutputDataConfig
import Amazonka.SageMaker.Types.AutoMLPartialFailureReason
import Amazonka.SageMaker.Types.AutoMLS3DataSource
import Amazonka.SageMaker.Types.AutoMLSecurityConfig
import Amazonka.SageMaker.Types.AutoRollbackConfig
import Amazonka.SageMaker.Types.Bias
import Amazonka.SageMaker.Types.BlueGreenUpdatePolicy
import Amazonka.SageMaker.Types.CacheHitResult
import Amazonka.SageMaker.Types.CallbackStepMetadata
import Amazonka.SageMaker.Types.CandidateArtifactLocations
import Amazonka.SageMaker.Types.CandidateProperties
import Amazonka.SageMaker.Types.CapacitySize
import Amazonka.SageMaker.Types.CaptureContentTypeHeader
import Amazonka.SageMaker.Types.CaptureOption
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.ChannelSpecification
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.CodeRepositorySummary
import Amazonka.SageMaker.Types.CognitoConfig
import Amazonka.SageMaker.Types.CognitoMemberDefinition
import Amazonka.SageMaker.Types.CollectionConfiguration
import Amazonka.SageMaker.Types.CompilationJobSummary
import Amazonka.SageMaker.Types.ConditionStepMetadata
import Amazonka.SageMaker.Types.ContainerDefinition
import Amazonka.SageMaker.Types.ContextSource
import Amazonka.SageMaker.Types.ContextSummary
import Amazonka.SageMaker.Types.ContinuousParameterRange
import Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification
import Amazonka.SageMaker.Types.CustomImage
import Amazonka.SageMaker.Types.DataCaptureConfig
import Amazonka.SageMaker.Types.DataCaptureConfigSummary
import Amazonka.SageMaker.Types.DataCatalogConfig
import Amazonka.SageMaker.Types.DataProcessing
import Amazonka.SageMaker.Types.DataQualityAppSpecification
import Amazonka.SageMaker.Types.DataQualityBaselineConfig
import Amazonka.SageMaker.Types.DataQualityJobInput
import Amazonka.SageMaker.Types.DataSource
import Amazonka.SageMaker.Types.DatasetDefinition
import Amazonka.SageMaker.Types.DebugHookConfig
import Amazonka.SageMaker.Types.DebugRuleConfiguration
import Amazonka.SageMaker.Types.DebugRuleEvaluationStatus
import Amazonka.SageMaker.Types.DeployedImage
import Amazonka.SageMaker.Types.DeploymentConfig
import Amazonka.SageMaker.Types.DesiredWeightAndCapacity
import Amazonka.SageMaker.Types.Device
import Amazonka.SageMaker.Types.DeviceFleetSummary
import Amazonka.SageMaker.Types.DeviceStats
import Amazonka.SageMaker.Types.DeviceSummary
import Amazonka.SageMaker.Types.DomainDetails
import Amazonka.SageMaker.Types.EdgeModel
import Amazonka.SageMaker.Types.EdgeModelStat
import Amazonka.SageMaker.Types.EdgeModelSummary
import Amazonka.SageMaker.Types.EdgeOutputConfig
import Amazonka.SageMaker.Types.EdgePackagingJobSummary
import Amazonka.SageMaker.Types.EdgePresetDeploymentOutput
import Amazonka.SageMaker.Types.Endpoint
import Amazonka.SageMaker.Types.EndpointConfigSummary
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.EndpointSummary
import Amazonka.SageMaker.Types.Experiment
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.ExperimentSource
import Amazonka.SageMaker.Types.ExperimentSummary
import Amazonka.SageMaker.Types.Explainability
import Amazonka.SageMaker.Types.FeatureDefinition
import Amazonka.SageMaker.Types.FeatureGroup
import Amazonka.SageMaker.Types.FeatureGroupSummary
import Amazonka.SageMaker.Types.FileSystemConfig
import Amazonka.SageMaker.Types.FileSystemDataSource
import Amazonka.SageMaker.Types.Filter
import Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Amazonka.SageMaker.Types.FlowDefinitionOutputConfig
import Amazonka.SageMaker.Types.FlowDefinitionSummary
import Amazonka.SageMaker.Types.GitConfig
import Amazonka.SageMaker.Types.GitConfigForUpdate
import Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig
import Amazonka.SageMaker.Types.HumanLoopActivationConfig
import Amazonka.SageMaker.Types.HumanLoopConfig
import Amazonka.SageMaker.Types.HumanLoopRequestSource
import Amazonka.SageMaker.Types.HumanTaskConfig
import Amazonka.SageMaker.Types.HumanTaskUiSummary
import Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification
import Amazonka.SageMaker.Types.HyperParameterSpecification
import Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition
import Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Amazonka.SageMaker.Types.Image
import Amazonka.SageMaker.Types.ImageConfig
import Amazonka.SageMaker.Types.ImageVersion
import Amazonka.SageMaker.Types.InferenceExecutionConfig
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.InputConfig
import Amazonka.SageMaker.Types.IntegerParameterRange
import Amazonka.SageMaker.Types.IntegerParameterRangeSpecification
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings
import Amazonka.SageMaker.Types.KernelGatewayImageConfig
import Amazonka.SageMaker.Types.KernelSpec
import Amazonka.SageMaker.Types.LabelCounters
import Amazonka.SageMaker.Types.LabelCountersForWorkteam
import Amazonka.SageMaker.Types.LabelingJobAlgorithmsConfig
import Amazonka.SageMaker.Types.LabelingJobDataAttributes
import Amazonka.SageMaker.Types.LabelingJobDataSource
import Amazonka.SageMaker.Types.LabelingJobForWorkteamSummary
import Amazonka.SageMaker.Types.LabelingJobInputConfig
import Amazonka.SageMaker.Types.LabelingJobOutput
import Amazonka.SageMaker.Types.LabelingJobOutputConfig
import Amazonka.SageMaker.Types.LabelingJobResourceConfig
import Amazonka.SageMaker.Types.LabelingJobS3DataSource
import Amazonka.SageMaker.Types.LabelingJobSnsDataSource
import Amazonka.SageMaker.Types.LabelingJobStoppingConditions
import Amazonka.SageMaker.Types.LabelingJobSummary
import Amazonka.SageMaker.Types.LambdaStepMetadata
import Amazonka.SageMaker.Types.MemberDefinition
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.MetricData
import Amazonka.SageMaker.Types.MetricDatum
import Amazonka.SageMaker.Types.MetricDefinition
import Amazonka.SageMaker.Types.MetricsSource
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.ModelBiasAppSpecification
import Amazonka.SageMaker.Types.ModelBiasBaselineConfig
import Amazonka.SageMaker.Types.ModelBiasJobInput
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelMetrics
import Amazonka.SageMaker.Types.ModelPackage
import Amazonka.SageMaker.Types.ModelPackageContainerDefinition
import Amazonka.SageMaker.Types.ModelPackageGroup
import Amazonka.SageMaker.Types.ModelPackageGroupSummary
import Amazonka.SageMaker.Types.ModelPackageStatusDetails
import Amazonka.SageMaker.Types.ModelPackageStatusItem
import Amazonka.SageMaker.Types.ModelPackageSummary
import Amazonka.SageMaker.Types.ModelPackageValidationProfile
import Amazonka.SageMaker.Types.ModelPackageValidationSpecification
import Amazonka.SageMaker.Types.ModelQuality
import Amazonka.SageMaker.Types.ModelQualityAppSpecification
import Amazonka.SageMaker.Types.ModelQualityBaselineConfig
import Amazonka.SageMaker.Types.ModelQualityJobInput
import Amazonka.SageMaker.Types.ModelStepMetadata
import Amazonka.SageMaker.Types.ModelSummary
import Amazonka.SageMaker.Types.MonitoringAppSpecification
import Amazonka.SageMaker.Types.MonitoringBaselineConfig
import Amazonka.SageMaker.Types.MonitoringClusterConfig
import Amazonka.SageMaker.Types.MonitoringConstraintsResource
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input
import Amazonka.SageMaker.Types.MonitoringInput
import Amazonka.SageMaker.Types.MonitoringJobDefinition
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSummary
import Amazonka.SageMaker.Types.MonitoringNetworkConfig
import Amazonka.SageMaker.Types.MonitoringOutput
import Amazonka.SageMaker.Types.MonitoringOutputConfig
import Amazonka.SageMaker.Types.MonitoringResources
import Amazonka.SageMaker.Types.MonitoringS3Output
import Amazonka.SageMaker.Types.MonitoringSchedule
import Amazonka.SageMaker.Types.MonitoringScheduleConfig
import Amazonka.SageMaker.Types.MonitoringScheduleSummary
import Amazonka.SageMaker.Types.MonitoringStatisticsResource
import Amazonka.SageMaker.Types.MonitoringStoppingCondition
import Amazonka.SageMaker.Types.MultiModelConfig
import Amazonka.SageMaker.Types.NeoVpcConfig
import Amazonka.SageMaker.Types.NestedFilters
import Amazonka.SageMaker.Types.NetworkConfig
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleHook
import Amazonka.SageMaker.Types.NotebookInstanceSummary
import Amazonka.SageMaker.Types.NotificationConfiguration
import Amazonka.SageMaker.Types.ObjectiveStatusCounters
import Amazonka.SageMaker.Types.OfflineStoreConfig
import Amazonka.SageMaker.Types.OfflineStoreStatus
import Amazonka.SageMaker.Types.OidcConfig
import Amazonka.SageMaker.Types.OidcConfigForResponse
import Amazonka.SageMaker.Types.OidcMemberDefinition
import Amazonka.SageMaker.Types.OnlineStoreConfig
import Amazonka.SageMaker.Types.OnlineStoreSecurityConfig
import Amazonka.SageMaker.Types.OutputConfig
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.OutputParameter
import Amazonka.SageMaker.Types.Parameter
import Amazonka.SageMaker.Types.ParameterRange
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.Parent
import Amazonka.SageMaker.Types.ParentHyperParameterTuningJob
import Amazonka.SageMaker.Types.Pipeline
import Amazonka.SageMaker.Types.PipelineExecution
import Amazonka.SageMaker.Types.PipelineExecutionStep
import Amazonka.SageMaker.Types.PipelineExecutionStepMetadata
import Amazonka.SageMaker.Types.PipelineExecutionSummary
import Amazonka.SageMaker.Types.PipelineExperimentConfig
import Amazonka.SageMaker.Types.PipelineSummary
import Amazonka.SageMaker.Types.ProcessingClusterConfig
import Amazonka.SageMaker.Types.ProcessingFeatureStoreOutput
import Amazonka.SageMaker.Types.ProcessingInput
import Amazonka.SageMaker.Types.ProcessingJob
import Amazonka.SageMaker.Types.ProcessingJobStepMetadata
import Amazonka.SageMaker.Types.ProcessingJobSummary
import Amazonka.SageMaker.Types.ProcessingOutput
import Amazonka.SageMaker.Types.ProcessingOutputConfig
import Amazonka.SageMaker.Types.ProcessingResources
import Amazonka.SageMaker.Types.ProcessingS3Input
import Amazonka.SageMaker.Types.ProcessingS3Output
import Amazonka.SageMaker.Types.ProcessingStoppingCondition
import Amazonka.SageMaker.Types.ProductionVariant
import Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig
import Amazonka.SageMaker.Types.ProductionVariantSummary
import Amazonka.SageMaker.Types.ProfilerConfig
import Amazonka.SageMaker.Types.ProfilerConfigForUpdate
import Amazonka.SageMaker.Types.ProfilerRuleConfiguration
import Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus
import Amazonka.SageMaker.Types.Project
import Amazonka.SageMaker.Types.ProjectSummary
import Amazonka.SageMaker.Types.PropertyNameQuery
import Amazonka.SageMaker.Types.PropertyNameSuggestion
import Amazonka.SageMaker.Types.ProvisioningParameter
import Amazonka.SageMaker.Types.PublicWorkforceTaskPrice
import Amazonka.SageMaker.Types.RedshiftDatasetDefinition
import Amazonka.SageMaker.Types.RegisterModelStepMetadata
import Amazonka.SageMaker.Types.RenderableTask
import Amazonka.SageMaker.Types.RenderingError
import Amazonka.SageMaker.Types.RepositoryAuthConfig
import Amazonka.SageMaker.Types.ResolvedAttributes
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.ResourceLimits
import Amazonka.SageMaker.Types.ResourceSpec
import Amazonka.SageMaker.Types.RetentionPolicy
import Amazonka.SageMaker.Types.RetryStrategy
import Amazonka.SageMaker.Types.S3DataSource
import Amazonka.SageMaker.Types.S3StorageConfig
import Amazonka.SageMaker.Types.ScheduleConfig
import Amazonka.SageMaker.Types.SearchExpression
import Amazonka.SageMaker.Types.SearchRecord
import Amazonka.SageMaker.Types.SecondaryStatusTransition
import Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.StudioLifecycleConfigDetails
import Amazonka.SageMaker.Types.SubscribedWorkteam
import Amazonka.SageMaker.Types.SuggestionQuery
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TargetPlatform
import Amazonka.SageMaker.Types.TensorBoardAppSettings
import Amazonka.SageMaker.Types.TensorBoardOutputConfig
import Amazonka.SageMaker.Types.TrafficRoutingConfig
import Amazonka.SageMaker.Types.TrainingJob
import Amazonka.SageMaker.Types.TrainingJobDefinition
import Amazonka.SageMaker.Types.TrainingJobStatusCounters
import Amazonka.SageMaker.Types.TrainingJobStepMetadata
import Amazonka.SageMaker.Types.TrainingJobSummary
import Amazonka.SageMaker.Types.TrainingSpecification
import Amazonka.SageMaker.Types.TransformDataSource
import Amazonka.SageMaker.Types.TransformInput
import Amazonka.SageMaker.Types.TransformJob
import Amazonka.SageMaker.Types.TransformJobDefinition
import Amazonka.SageMaker.Types.TransformJobStepMetadata
import Amazonka.SageMaker.Types.TransformJobSummary
import Amazonka.SageMaker.Types.TransformOutput
import Amazonka.SageMaker.Types.TransformResources
import Amazonka.SageMaker.Types.TransformS3DataSource
import Amazonka.SageMaker.Types.Trial
import Amazonka.SageMaker.Types.TrialComponent
import Amazonka.SageMaker.Types.TrialComponentArtifact
import Amazonka.SageMaker.Types.TrialComponentMetricSummary
import Amazonka.SageMaker.Types.TrialComponentParameterValue
import Amazonka.SageMaker.Types.TrialComponentSimpleSummary
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.TrialComponentSourceDetail
import Amazonka.SageMaker.Types.TrialComponentStatus
import Amazonka.SageMaker.Types.TrialComponentSummary
import Amazonka.SageMaker.Types.TrialSource
import Amazonka.SageMaker.Types.TrialSummary
import Amazonka.SageMaker.Types.TuningJobCompletionCriteria
import Amazonka.SageMaker.Types.TuningJobStepMetaData
import Amazonka.SageMaker.Types.USD
import Amazonka.SageMaker.Types.UiConfig
import Amazonka.SageMaker.Types.UiTemplate
import Amazonka.SageMaker.Types.UiTemplateInfo
import Amazonka.SageMaker.Types.UserContext
import Amazonka.SageMaker.Types.UserProfileDetails
import Amazonka.SageMaker.Types.UserSettings
import Amazonka.SageMaker.Types.VariantProperty
import Amazonka.SageMaker.Types.VpcConfig
import Amazonka.SageMaker.Types.Workforce
import Amazonka.SageMaker.Types.Workteam
import Amazonka.SageMaker.UpdateAction
import Amazonka.SageMaker.UpdateAppImageConfig
import Amazonka.SageMaker.UpdateArtifact
import Amazonka.SageMaker.UpdateCodeRepository
import Amazonka.SageMaker.UpdateContext
import Amazonka.SageMaker.UpdateDeviceFleet
import Amazonka.SageMaker.UpdateDevices
import Amazonka.SageMaker.UpdateDomain
import Amazonka.SageMaker.UpdateEndpoint
import Amazonka.SageMaker.UpdateEndpointWeightsAndCapacities
import Amazonka.SageMaker.UpdateExperiment
import Amazonka.SageMaker.UpdateImage
import Amazonka.SageMaker.UpdateModelPackage
import Amazonka.SageMaker.UpdateMonitoringSchedule
import Amazonka.SageMaker.UpdateNotebookInstance
import Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.UpdatePipeline
import Amazonka.SageMaker.UpdatePipelineExecution
import Amazonka.SageMaker.UpdateTrainingJob
import Amazonka.SageMaker.UpdateTrial
import Amazonka.SageMaker.UpdateTrialComponent
import Amazonka.SageMaker.UpdateUserProfile
import Amazonka.SageMaker.UpdateWorkforce
import Amazonka.SageMaker.UpdateWorkteam
